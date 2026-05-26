;;; org-github.el --- GitHub Issues/PRs integration with Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Bala Ramadurai
;; Author: Bala Ramadurai <bala@balaramadurai.net>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: convenience tools vc
;; URL: https://github.com/balaramadurai/org-github

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; org-github provides integration between GitHub issues/PRs and Org-mode,
;; with built-in time tracking support.
;;
;; Features:
;; - Fetch GitHub issues and PRs into Org headings with full metadata
;; - Clock in/out on issues with optional time logging to GitHub
;; - Sync issue/PR states between GitHub and Org-mode
;; - Create issues from Org headings
;; - Add comments to issues/PRs from Emacs
;; - Extract and estimate time from git commit history
;; - PARA-compatible file organization via repo-to-file mappings
;;
;; Requirements:
;; - GitHub CLI (gh) must be installed and authenticated
;; - Emacs 27.1 or later
;; - Org-mode 9.0 or later

;;; Code:

(require 'org)
(require 'org-clock)
(require 'json)
(require 'subr-x)

(defvar org-state)  ; dynamically bound by org-todo

(defgroup org-github nil
  "Settings for org-github."
  :group 'org
  :prefix "org-github-")

(defcustom org-github-org-file (expand-file-name "github.org" org-directory)
  "Default path to Org file for GitHub issues.
Used when no specific mapping exists in `org-github-repo-file-alist'."
  :type 'string
  :group 'org-github)

(defcustom org-github-default-repos nil
  "Default repositories to track.
List of \"owner/repo\" strings."
  :type '(repeat string)
  :group 'org-github)

(defcustom org-github-repo-file-alist nil
  "Alist mapping repositories to their target org files and headings.
Each entry is (REPO . (FILE . HEADING)) where:
  REPO is \"owner/repo\" string
  FILE is the org file path
  HEADING is the parent heading under which issues are placed (or nil for top-level)

Example:
  \\='((\"owner/project-repo\" . (\"~/Documents/1Projects/projects.org\" . \"PROJECT MyProject\"))
    (\"owner/area-repo\" . (\"~/Documents/2Areas/myarea.org\" . \"TODO MyArea\")))"
  :type '(alist :key-type string
                :value-type (cons string (choice string null)))
  :group 'org-github)

(defcustom org-github-auto-clock t
  "Auto clock in when starting work on an issue."
  :type 'boolean
  :group 'org-github)

(defcustom org-github-log-to-github nil
  "Log time to GitHub as a comment when clocking out."
  :type 'boolean
  :group 'org-github)

(defcustom org-github-process-timeout 60
  "Timeout in seconds for GitHub CLI operations."
  :type 'integer
  :group 'org-github)

(defcustom org-github-issue-todo-state "TODO"
  "TODO state for open issues."
  :type 'string
  :group 'org-github)

(defcustom org-github-pr-todo-state "TODO"
  "TODO state for open PRs."
  :type 'string
  :group 'org-github)

(defcustom org-github-closed-state "DONE"
  "State for closed issues."
  :type 'string
  :group 'org-github)

(defcustom org-github-pr-closed-state "CANCELLED"
  "State for PRs closed without merging."
  :type 'string
  :group 'org-github)

(defcustom org-github-pr-merged-state "DONE"
  "State for merged PRs."
  :type 'string
  :group 'org-github)

(defcustom org-github-open-substates '("NEXT" "WAITING")
  "Org TODO states that are sub-states of GitHub \"open\".
When an issue is open on GitHub and the local heading uses one of
these states, the sync will not overwrite it back to the default
open state (e.g. TODO).  Closing the issue on GitHub will still
change the state to `org-github-closed-state'."
  :type '(repeat string)
  :group 'org-github)

(defcustom org-github-repo-project-alist nil
  "Alist mapping repos to GitHub Projects V2 numbers and owners.
Each entry is (REPO . (OWNER . PROJECT-NUMBER)).
When set, deadlines from the project board are synced as Org DEADLINE."
  :type '(alist :key-type string
                :value-type (cons string integer))
  :group 'org-github)

(defcustom org-github-project-deadlines-cache-ttl 300
  "Seconds before cached GitHub Projects V2 deadlines are considered stale.
Set to 0 to disable caching and always fetch from GitHub."
  :type 'integer
  :group 'org-github)

;;; Internal Functions

(defvar org-github--project-deadlines-cache (make-hash-table :test 'equal)
  "Cache for GitHub Projects V2 deadline data, keyed by repo string.
Each value is (FETCH-TIME . DEADLINES-ALIST).")

(defvar org-github--active-syncs (make-hash-table :test 'equal)
  "Hash table of (REPO . NUM-STR) keys for syncs currently in progress.
Prevents duplicate concurrent async syncs for the same item.")

(defun org-github--gh-available-p ()
  "Check if gh CLI is available."
  (executable-find "gh"))

(defun org-github--run-gh-sync (args)
  "Run gh CLI with ARGS, blocking until done but yielding to the event loop.
Yielding allows timer-based spinner animations to fire during the wait."
  (unless (org-github--gh-available-p)
    (error "GitHub CLI (gh) not found. Install from https://cli.github.com/"))
  (let* ((buf (generate-new-buffer " *org-github-gh-sync*"))
         (proc (apply #'start-process "org-github-gh" buf "gh" args)))
    (while (process-live-p proc)
      (accept-process-output proc 0.05))
    (with-current-buffer buf
      (let ((exit-code (process-exit-status proc))
            (output (buffer-string)))
        (kill-buffer buf)
        (if (zerop exit-code)
            output
          (error "gh failed: %s" output))))))

(defun org-github--parse-json (json-string)
  "Parse JSON-STRING into elisp data structures."
  (let ((json-array-type 'list)
        (json-object-type 'alist)
        (json-key-type 'symbol))
    (condition-case nil
        (json-read-from-string json-string)
      (error nil))))

(defun org-github--format-time (time-string)
  "Format TIME-STRING to active Org timestamp."
  (when (and time-string (not (string-empty-p time-string)))
    (format-time-string "<%Y-%m-%d %a %H:%M>" (date-to-time time-string))))

(defun org-github--format-time-plain (time-string)
  "Format TIME-STRING to inactive Org timestamp.
Uses inactive timestamps to avoid agenda clutter."
  (when (and time-string (not (string-empty-p time-string)))
    (format-time-string "[%Y-%m-%d %a %H:%M]" (date-to-time time-string))))

(defun org-github--state-to-todo (state type)
  "Convert GitHub STATE to Org TODO keyword based on TYPE.
TYPE is either \\='issue or \\='pr.
STATE is case-insensitive (gh CLI returns uppercase)."
  (pcase (downcase state)
    ("open" (if (eq type 'pr) org-github-pr-todo-state org-github-issue-todo-state))
    ("closed" (if (eq type 'pr) org-github-pr-closed-state org-github-closed-state))
    ("merged" org-github-pr-merged-state)
    (_ org-github-issue-todo-state)))

;;; GitHub API Functions

(defun org-github--fetch-issues (repo &optional state)
  "Fetch issues from REPO with optional STATE filter.
STATE can be \"open\", \"closed\", or \"all\" (default)."
  (let* ((state-arg (or state "all"))
         (json-output (org-github--run-gh-sync
                       (list "issue" "list" "-R" repo
                             "--state" state-arg
                             "--json" "number,title,body,state,createdAt,updatedAt,closedAt,labels,assignees,milestone,url,author"
                             "--limit" "1000"))))
    (org-github--parse-json json-output)))

(defun org-github--fetch-prs (repo &optional state)
  "Fetch PRs from REPO with optional STATE filter.
STATE can be \"open\", \"closed\", \"merged\", or \"all\" (default)."
  (let* ((state-arg (or state "all"))
         (json-output (org-github--run-gh-sync
                       (list "pr" "list" "-R" repo
                             "--state" state-arg
                             "--json" "number,title,body,state,createdAt,updatedAt,closedAt,mergedAt,labels,assignees,milestone,url,author,headRefName,baseRefName"
                             "--limit" "1000"))))
    (org-github--parse-json json-output)))

(defun org-github-clear-project-deadlines-cache ()
  "Clear cached GitHub Projects V2 deadline data for all repos."
  (interactive)
  (clrhash org-github--project-deadlines-cache)
  (message "org-github: project deadlines cache cleared"))

(defun org-github--fetch-project-deadlines (repo)
  "Fetch deadline field values from GitHub Projects V2 for REPO.
Returns an alist of (ISSUE-NUMBER . \"YYYY-MM-DD\") for issues that have
a Deadline field set.  Uses `org-github-repo-project-alist' to find
the project owner and number.  Paginates through all project items.
Results are cached for `org-github-project-deadlines-cache-ttl' seconds."
  (let* ((entry (gethash repo org-github--project-deadlines-cache))
         (fetch-time (car entry))
         (cached-deadlines (cdr entry)))
    (if (and (> org-github-project-deadlines-cache-ttl 0)
             fetch-time
             (< (float-time (time-subtract (current-time) fetch-time))
                org-github-project-deadlines-cache-ttl))
        cached-deadlines
      (let ((project-config (cdr (assoc repo org-github-repo-project-alist))))
        (when project-config
          (let* ((owner (car project-config))
                 (project-num (cdr project-config))
                 (deadlines '())
                 (has-next t)
                 (cursor nil))
            (while has-next
              (let* ((after-clause (if cursor
                                       (format "after: \"%s\"" cursor)
                                     ""))
                     (query (format "{
  user(login: \"%s\") {
    projectV2(number: %d) {
      items(first: 100 %s) {
        pageInfo { hasNextPage endCursor }
        nodes {
          content {
            ... on Issue {
              number
              repository { nameWithOwner }
            }
            ... on PullRequest {
              number
              repository { nameWithOwner }
            }
          }
          fieldValueByName(name: \"Deadline\") {
            ... on ProjectV2ItemFieldDateValue {
              date
            }
          }
        }
      }
    }
  }
}" owner project-num after-clause))
                     (json-output (org-github--run-gh-sync
                                   (list "api" "graphql" "-f" (concat "query=" query))))
                     (data (org-github--parse-json json-output))
                     (items (alist-get 'items
                                       (alist-get 'projectV2
                                                  (alist-get 'user
                                                             (alist-get 'data data)))))
                     (page-info (alist-get 'pageInfo items))
                     (nodes (alist-get 'nodes items)))
                (dolist (node nodes)
                  (let* ((content (alist-get 'content node))
                         (number (alist-get 'number content))
                         (node-repo (alist-get 'nameWithOwner (alist-get 'repository content)))
                         (deadline-field (alist-get 'fieldValueByName node))
                         (date (when deadline-field (alist-get 'date deadline-field))))
                    (when (and number date (string= node-repo repo))
                      (push (cons number date) deadlines))))
                (setq has-next (eq (alist-get 'hasNextPage page-info) t))
                (setq cursor (alist-get 'endCursor page-info))))
            (when (> org-github-project-deadlines-cache-ttl 0)
              (puthash repo (cons (current-time) deadlines)
                       org-github--project-deadlines-cache))
            (message "Fetched %d deadlines from project for %s" (length deadlines) repo)
            deadlines))))))

(defun org-github--sanitize-tag (name)
  "Sanitize GitHub label NAME for use as an Org tag.
Replaces spaces, hyphens, and other invalid tag characters with underscores."
  (replace-regexp-in-string "[^[:alnum:]_@#%]" "_" name))

;;; Org Conversion Functions

(defun org-github--issue-to-org (issue repo)
  "Convert ISSUE to Org heading for REPO.
Issues are created at level 3 (***) to be subtrees under GitHub Issues heading.
If ISSUE contains a `deadline' key, it is added as an Org DEADLINE."
  (let* ((number (alist-get 'number issue))
         (title (alist-get 'title issue))
         (body (or (alist-get 'body issue) ""))
         (state (alist-get 'state issue))
         (url (alist-get 'url issue))
         (created (alist-get 'createdAt issue))
         (updated (alist-get 'updatedAt issue))
         (closed (alist-get 'closedAt issue))
         (author (alist-get 'login (alist-get 'author issue)))
         (labels (mapcar (lambda (l) (org-github--sanitize-tag (alist-get 'name l))) (alist-get 'labels issue)))
         (assignees (mapcar (lambda (a) (alist-get 'login a)) (alist-get 'assignees issue)))
         (milestone (alist-get 'title (alist-get 'milestone issue)))
         (deadline (alist-get 'deadline issue))
         (todo-state (org-github--state-to-todo state 'issue))
         (tags (if labels (concat ":" (string-join labels ":") ":") ""))
         (body-text (string-trim body)))
    (concat
     (format "*** %s #%d %s" todo-state number title)
     (if (string-empty-p tags) "" (format " %s" tags))
     "\n"
     (if deadline
         (format "DEADLINE: <%s>\n"
                 (format-time-string "%Y-%m-%d %a"
                                     (date-to-time (concat deadline "T00:00:00Z"))))
       "")
     ":PROPERTIES:\n"
     (format ":ISSUE_NUMBER: %d\n" number)
     (format ":REPO: %s\n" repo)
     (format ":STATE: %s\n" state)
     (format ":URL: %s\n" url)
     (format ":AUTHOR: %s\n" (or author ""))
     (format ":CREATED_AT: %s\n" (or (org-github--format-time-plain created) ""))
     (format ":UPDATED_AT: %s\n" (or (org-github--format-time-plain updated) ""))
     (if closed (format ":CLOSED_AT: %s\n" (org-github--format-time-plain closed)) "")
     (if assignees (format ":ASSIGNEES: %s\n" (string-join assignees ", ")) "")
     (if milestone (format ":MILESTONE: %s\n" milestone) "")
     ":END:\n"
     (if (string-empty-p body-text) "" (concat "\n" body-text "\n"))
     "\n")))

(defun org-github--pr-to-org (pr repo)
  "Convert PR to Org heading for REPO.
PRs are created at level 3 (***) to be subtrees under GitHub Issues heading."
  (let* ((number (alist-get 'number pr))
         (title (alist-get 'title pr))
         (body (or (alist-get 'body pr) ""))
         (state (alist-get 'state pr))
         (url (alist-get 'url pr))
         (created (alist-get 'createdAt pr))
         (updated (alist-get 'updatedAt pr))
         (merged (alist-get 'mergedAt pr))
         (closed (alist-get 'closedAt pr))
         (author (alist-get 'login (alist-get 'author pr)))
         (head-ref (alist-get 'headRefName pr))
         (base-ref (alist-get 'baseRefName pr))
         (deadline (alist-get 'deadline pr))
         (labels (mapcar (lambda (l) (org-github--sanitize-tag (alist-get 'name l))) (alist-get 'labels pr)))
         (assignees (mapcar (lambda (a) (alist-get 'login a)) (alist-get 'assignees pr)))
         (milestone (alist-get 'title (alist-get 'milestone pr)))
         (todo-state (org-github--state-to-todo (if merged "merged" state) 'pr))
         (tags (concat ":PR:" (if labels (concat (string-join labels ":") ":") "")))
         (body-text (string-trim body)))
    (concat
     (format "*** %s PR#%d %s %s\n" todo-state number title tags)
     (if deadline
         (format "DEADLINE: <%s>\n"
                 (format-time-string "%Y-%m-%d %a"
                                     (date-to-time (concat deadline "T00:00:00Z"))))
       "")
     ":PROPERTIES:\n"
     (format ":PR_NUMBER: %d\n" number)
     (format ":REPO: %s\n" repo)
     (format ":STATE: %s\n" state)
     (format ":URL: %s\n" url)
     (format ":AUTHOR: %s\n" (or author ""))
     (format ":HEAD_REF: %s\n" (or head-ref ""))
     (format ":BASE_REF: %s\n" (or base-ref ""))
     (format ":CREATED_AT: %s\n" (or (org-github--format-time-plain created) ""))
     (format ":UPDATED_AT: %s\n" (or (org-github--format-time-plain updated) ""))
     (if merged (format ":MERGED_AT: %s\n" (org-github--format-time-plain merged)) "")
     (if closed (format ":CLOSED_AT: %s\n" (org-github--format-time-plain closed)) "")
     (if assignees (format ":ASSIGNEES: %s\n" (string-join assignees ", ")) "")
     (if milestone (format ":MILESTONE: %s\n" milestone) "")
     ":END:\n"
     (if (string-empty-p body-text) "" (concat "\n" body-text "\n"))
     "\n")))

;;; File/Heading Management

(defun org-github--get-repo-config (repo)
  "Get the file and heading configuration for REPO.
Returns (FILE . HEADING) or nil if not found in alist."
  (cdr (assoc repo org-github-repo-file-alist)))

(defun org-github--get-repo-file (repo)
  "Get the org file for REPO from alist or default."
  (let ((config (org-github--get-repo-config repo)))
    (expand-file-name (if config (car config) org-github-org-file))))

(defun org-github--find-or-create-repo-heading (repo)
  "Find or create heading for REPO.
Uses `org-github-repo-file-alist' to determine file and parent heading."
  (let* ((config (org-github--get-repo-config repo))
         (org-file (expand-file-name (if config (car config) org-github-org-file)))
         (parent-heading (when config (cdr config))))
    (unless (file-exists-p org-file)
      (with-temp-file org-file
        (insert "#+TITLE: GitHub Issues & PRs\n#+FILETAGS: :github:\n\n")))
    (with-current-buffer (find-file-noselect org-file)
      (widen)
      (goto-char (point-min))
      (if parent-heading
          ;; Find the parent heading first, then look for GitHub Issues subheading
          (if (re-search-forward (format "^\\*+ %s" (regexp-quote parent-heading)) nil t)
              (let ((parent-level (org-current-level))
                    (subtree-end (save-excursion (org-end-of-subtree t t) (point))))
                ;; Look for "GitHub Issues" subheading under parent
                ;; Allow TODO keywords and tags between stars and "GitHub Issues"
                (if (re-search-forward
                     (format "^\\*\\{%d\\} \\(?:[A-Z]+ \\)?GitHub Issues" (1+ parent-level))
                     subtree-end t)
                    (progn
                      (org-end-of-subtree t t)
                      (unless (bolp) (insert "\n"))
                      (point))
                  ;; Create GitHub Issues subheading
                  (goto-char subtree-end)
                  (unless (bolp) (insert "\n"))
                  (insert (format "%s GitHub Issues\n:PROPERTIES:\n:REPO: %s\n:END:\n\n"
                                  (make-string (1+ parent-level) ?*)
                                  repo))
                  (point)))
            ;; Parent heading not found, create it
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (format "* %s\n:PROPERTIES:\n:CATEGORY: GitHub\n:END:\n" parent-heading))
            (insert (format "** GitHub Issues\n:PROPERTIES:\n:REPO: %s\n:END:\n\n" repo))
            (point))
        ;; No parent heading specified, use old behavior (top-level repo heading)
        (if (re-search-forward (format "^\\* %s$" (regexp-quote repo)) nil t)
            (progn
              (org-end-of-subtree t t)
              (unless (bolp) (insert "\n"))
              (point))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert (format "* %s\n:PROPERTIES:\n:REPO: %s\n:END:\n\n" repo repo))
          (point))))))

;;; Issue/PR Detection

(defun org-github--issue-exists-p (repo number)
  "Check if issue NUMBER from REPO exists in the org file."
  (let ((org-file (org-github--get-repo-file repo)))
    (when (file-exists-p org-file)
      (with-current-buffer (find-file-noselect org-file)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (catch 'found
              (while (re-search-forward (format ":ISSUE_NUMBER: %d$" number) nil t)
                (save-excursion
                  (org-back-to-heading t)
                  (when (string= repo (org-entry-get (point) "REPO"))
                    (throw 'found t))))
              nil)))))))

(defun org-github--pr-exists-p (repo number)
  "Check if PR NUMBER from REPO exists in the org file."
  (let ((org-file (org-github--get-repo-file repo)))
    (when (file-exists-p org-file)
      (with-current-buffer (find-file-noselect org-file)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (catch 'found
              (while (re-search-forward (format ":PR_NUMBER: %d$" number) nil t)
                (save-excursion
                  (org-back-to-heading t)
                  (when (string= repo (org-entry-get (point) "REPO"))
                    (throw 'found t))))
              nil)))))))

(defun org-github--find-issue-heading (repo number)
  "Find and return position of issue NUMBER from REPO, or nil if not found."
  (let ((org-file (org-github--get-repo-file repo)))
    (when (file-exists-p org-file)
      (with-current-buffer (find-file-noselect org-file)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (catch 'found
              (while (re-search-forward (format ":ISSUE_NUMBER: %d$" number) nil t)
                (save-excursion
                  (org-back-to-heading t)
                  (when (string= repo (org-entry-get (point) "REPO"))
                    (throw 'found (point)))))
              nil)))))))

(defun org-github--find-pr-heading (repo number)
  "Find and return position of PR NUMBER from REPO, or nil if not found."
  (let ((org-file (org-github--get-repo-file repo)))
    (when (file-exists-p org-file)
      (with-current-buffer (find-file-noselect org-file)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (catch 'found
              (while (re-search-forward (format ":PR_NUMBER: %d$" number) nil t)
                (save-excursion
                  (org-back-to-heading t)
                  (when (string= repo (org-entry-get (point) "REPO"))
                    (throw 'found (point)))))
              nil)))))))

;;; State Synchronization

(defun org-github--update-issue-state (repo number github-state &optional deadline issue)
  "Update org-mode TODO state for issue NUMBER from REPO based on GITHUB-STATE.
Optional DEADLINE is a \"YYYY-MM-DD\" string from GitHub Projects.
Optional ISSUE is the full issue alist for updating metadata like assignees."
  (let ((pos (org-github--find-issue-heading repo number))
        (org-file (org-github--get-repo-file repo)))
    (when pos
      (with-current-buffer (find-file-noselect org-file)
        (save-restriction
          (widen)
          (goto-char pos)
          (org-back-to-heading t)
          (let* ((current-state (org-entry-get (point) "STATE"))
                 (new-todo (org-github--state-to-todo github-state 'issue))
                 (current-todo (org-get-todo-state))
                 (state-changed (not (string= (downcase (or current-state ""))
                                               (downcase github-state))))
                 ;; Respect open sub-states: if GitHub says open and user set
                 ;; NEXT/WAITING, don't overwrite back to TODO
                 (open-substate-p (and (string= (downcase github-state) "open")
                                       (member current-todo org-github-open-substates)))
                 (todo-wrong (and (not open-substate-p)
                                  current-todo
                                  (not (string= current-todo new-todo))))
                 (changed nil))
            (when (or state-changed todo-wrong)
              (org-set-property "STATE" github-state)
              (org-back-to-heading t)
              (unless open-substate-p
                (org-todo new-todo))
              (setq changed t))
            ;; Update DEADLINE if provided
            (when deadline
              (org-back-to-heading t)
              (let ((dl-str (format-time-string "<%Y-%m-%d %a>"
                                                (date-to-time (concat deadline "T00:00:00Z")))))
                (unless (string= (or (org-entry-get (point) "DEADLINE") "") dl-str)
                  (org-deadline nil dl-str)
                  (setq changed t))))
            ;; Update metadata from full issue data
            (when issue
              (org-back-to-heading t)
              ;; Sync assignees
              (let* ((assignees (mapcar (lambda (a) (alist-get 'login a))
                                        (alist-get 'assignees issue)))
                     (new-assignees (if assignees (string-join assignees ", ") nil))
                     (current-assignees (org-entry-get (point) "ASSIGNEES")))
                (when (not (equal new-assignees current-assignees))
                  (if new-assignees
                      (org-set-property "ASSIGNEES" new-assignees)
                    (org-delete-property "ASSIGNEES"))
                  (setq changed t)))
              ;; Sync labels as tags (sanitized for Org) + GITHUB_LABELS (raw names for push)
              (org-back-to-heading t)
              (let* ((raw-labels (mapcar (lambda (l) (alist-get 'name l))
                                         (alist-get 'labels issue)))
                     (labels (mapcar #'org-github--sanitize-tag raw-labels))
                     (new-tags (if labels (concat ":" (string-join labels ":") ":") nil))
                     (current-tags (org-get-tags nil t))
                     (current-tag-str (if current-tags
                                          (concat ":" (string-join current-tags ":") ":")
                                        nil)))
                (when (not (equal new-tags current-tag-str))
                  (if labels
                      (org-set-tags (string-join labels ":"))
                    (org-set-tags nil))
                  (setq changed t))
                (let ((labels-str (string-join raw-labels ",")))
                  (unless (equal labels-str (org-entry-get (point) "GITHUB_LABELS"))
                    (if raw-labels
                        (org-set-property "GITHUB_LABELS" labels-str)
                      (org-delete-property "GITHUB_LABELS"))
                    (setq changed t))))
              ;; Sync milestone
              (org-back-to-heading t)
              (let* ((milestone (alist-get 'title (alist-get 'milestone issue)))
                     (current-milestone (org-entry-get (point) "MILESTONE")))
                (when (not (equal milestone current-milestone))
                  (if milestone
                      (org-set-property "MILESTONE" milestone)
                    (when current-milestone
                      (org-delete-property "MILESTONE")))
                  (setq changed t)))
              ;; Sync timestamps from GitHub
              (org-back-to-heading t)
              (let ((updated (org-github--format-time-plain
                              (alist-get 'updatedAt issue)))
                    (closed (org-github--format-time-plain
                             (alist-get 'closedAt issue))))
                (when updated
                  (org-set-property "UPDATED_AT" updated))
                (if closed
                    (org-set-property "CLOSED_AT" closed)
                  (when (org-entry-get (point) "CLOSED_AT")
                    (org-delete-property "CLOSED_AT")))))
            changed))))))

(defun org-github--update-pr-state (repo number github-state merged &optional pr deadline)
  "Update org-mode TODO state for PR NUMBER from REPO.
Uses GITHUB-STATE and MERGED timestamp to determine final state.
Optional PR is the full PR alist for updating timestamps.
Optional DEADLINE is a \"YYYY-MM-DD\" string from GitHub Projects."
  (let ((pos (org-github--find-pr-heading repo number))
        (org-file (org-github--get-repo-file repo)))
    (when pos
      (with-current-buffer (find-file-noselect org-file)
        (save-restriction
          (widen)
          (goto-char pos)
          (org-back-to-heading t)
          (let* ((current-state (org-entry-get (point) "STATE"))
                 (effective-state (if merged "merged" github-state))
                 (new-todo (org-github--state-to-todo effective-state 'pr))
                 (current-todo (org-get-todo-state))
                 (state-changed (not (string= (downcase (or current-state ""))
                                               (downcase effective-state))))
                 (todo-wrong (and current-todo
                                  (not (string= current-todo new-todo))))
                 (changed nil))
            (when (or state-changed todo-wrong)
              (org-set-property "STATE" effective-state)
              (when merged
                (org-set-property "MERGED_AT" (org-github--format-time-plain merged)))
              (org-back-to-heading t)
              (org-todo new-todo)
              (setq changed t))
            ;; Sync metadata from full PR data
            (when pr
              ;; Sync assignees
              (org-back-to-heading t)
              (let* ((assignees (mapcar (lambda (a) (alist-get 'login a))
                                        (alist-get 'assignees pr)))
                     (new-assignees (if assignees (string-join assignees ", ") nil))
                     (current-assignees (org-entry-get (point) "ASSIGNEES")))
                (when (not (equal new-assignees current-assignees))
                  (if new-assignees
                      (org-set-property "ASSIGNEES" new-assignees)
                    (org-delete-property "ASSIGNEES"))
                  (setq changed t)))
              ;; Sync labels as tags (sanitized for Org) + GITHUB_LABELS (raw names for push)
              (org-back-to-heading t)
              (let* ((raw-labels (mapcar (lambda (l) (alist-get 'name l))
                                         (alist-get 'labels pr)))
                     (labels (mapcar #'org-github--sanitize-tag raw-labels))
                     (new-tags (if labels (concat ":" (string-join labels ":") ":") nil))
                     (current-tags (org-get-tags nil t))
                     (current-tag-str (if current-tags
                                          (concat ":" (string-join current-tags ":") ":")
                                        nil)))
                (when (not (equal new-tags current-tag-str))
                  (if labels
                      (org-set-tags (string-join labels ":"))
                    (org-set-tags nil))
                  (setq changed t))
                (let ((labels-str (string-join raw-labels ",")))
                  (unless (equal labels-str (org-entry-get (point) "GITHUB_LABELS"))
                    (if raw-labels
                        (org-set-property "GITHUB_LABELS" labels-str)
                      (org-delete-property "GITHUB_LABELS"))
                    (setq changed t))))
              ;; Sync milestone
              (org-back-to-heading t)
              (let* ((milestone (alist-get 'title (alist-get 'milestone pr)))
                     (current-milestone (org-entry-get (point) "MILESTONE")))
                (when (not (equal milestone current-milestone))
                  (if milestone
                      (org-set-property "MILESTONE" milestone)
                    (when current-milestone
                      (org-delete-property "MILESTONE")))
                  (setq changed t)))
              ;; Sync timestamps
              (org-back-to-heading t)
              (let ((updated (org-github--format-time-plain
                              (alist-get 'updatedAt pr)))
                    (closed (org-github--format-time-plain
                             (alist-get 'closedAt pr))))
                (when updated
                  (org-set-property "UPDATED_AT" updated))
                (if closed
                    (org-set-property "CLOSED_AT" closed)
                  (when (org-entry-get (point) "CLOSED_AT")
                    (org-delete-property "CLOSED_AT")))))
            ;; Update DEADLINE if provided
            (when deadline
              (org-back-to-heading t)
              (let ((dl-str (format-time-string "<%Y-%m-%d %a>"
                                                (date-to-time (concat deadline "T00:00:00Z")))))
                (unless (string= (or (org-entry-get (point) "DEADLINE") "") dl-str)
                  (org-deadline nil dl-str)
                  (setq changed t))))
            changed))))))

;;; Interactive Commands

(defun org-github--fetch-single-issue (repo number)
  "Fetch a single issue NUMBER from REPO via gh CLI.
Returns the parsed alist or nil on failure."
  (let ((json-output (org-github--run-gh-sync
                      (list "issue" "view" (number-to-string number)
                            "-R" repo
                            "--json" "number,title,body,state,createdAt,updatedAt,closedAt,labels,assignees,milestone,url,author"))))
    (org-github--parse-json json-output)))

(defun org-github--fetch-single-pr (repo number)
  "Fetch a single PR NUMBER from REPO via gh CLI.
Returns the parsed alist or nil on failure."
  (let ((json-output (org-github--run-gh-sync
                      (list "pr" "view" (number-to-string number)
                            "-R" repo
                            "--json" "number,title,body,state,createdAt,updatedAt,closedAt,mergedAt,labels,assignees,milestone,url,author,headRefName,baseRefName"))))
    (org-github--parse-json json-output)))

(defun org-github--parse-updated-at (timestamp-str)
  "Parse TIMESTAMP-STR from either Org inactive format or ISO 8601.
Returns a time value for comparison, or 0 epoch if nil/unparseable."
  (if (or (null timestamp-str) (string-empty-p timestamp-str))
      (encode-time 0 0 0 1 1 1970)
    (condition-case nil
        (cond
         ;; Org inactive timestamp: [2025-01-20 Mon 14:22]
         ;; Written by `org-github--format-time-plain' in LOCAL time, so parse
         ;; it as local (no trailing Z) to round-trip correctly; otherwise a
         ;; non-UTC offset makes local look newer/older than it really is.
         ((string-match "\\[\\([0-9-]+\\)\\s-+\\w+\\s-+\\([0-9:]+\\)\\]" timestamp-str)
          (date-to-time (concat (match-string 1 timestamp-str) "T"
                                (match-string 2 timestamp-str) ":00")))
         ;; ISO 8601: 2025-01-20T14:22:00Z
         ((string-match "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T" timestamp-str)
          (date-to-time timestamp-str))
         (t (encode-time 0 0 0 1 1 1970)))
      (error (encode-time 0 0 0 1 1 1970)))))

(defun org-github--fetch-single-deadline (repo number)
  "Fetch the deadline for item NUMBER in REPO from GitHub Projects V2.
Returns a \"YYYY-MM-DD\" string or nil.  Skips fetch if no project
is configured for REPO in `org-github-repo-project-alist'."
  (when (assoc repo org-github-repo-project-alist)
    (cdr (assq number (org-github--fetch-project-deadlines repo)))))

(defun org-github--pull-at-point (repo num type remote-data)
  "Pull remote data for TYPE item NUM in REPO into org heading at point.
NUM must be an integer.  REMOTE-DATA is the parsed alist from GitHub.
Updates the heading in place using the existing update functions."
  (let* ((org-github--syncing t)
         (deadline (org-github--fetch-single-deadline repo num)))
    (if (string= type "issue")
        (let ((github-state (alist-get 'state remote-data)))
          (org-github--update-issue-state repo num
                                          (downcase github-state) deadline remote-data))
      ;; PR
      (let ((github-state (alist-get 'state remote-data))
            (merged (alist-get 'mergedAt remote-data)))
        (org-github--update-pr-state repo num
                                     (downcase github-state) merged remote-data deadline)))))

(defun org-github--push-at-point (repo num type &optional remote-data)
  "Push local org heading state to GitHub for TYPE item NUM in REPO.
Reads local properties and posts them to GitHub.
REMOTE-DATA, if provided, is used to skip pushes when values are unchanged."
  (let* ((org-github--syncing t)
         (changes '())
         (todo-state (org-get-todo-state))
         (local-state (org-entry-get (point) "STATE"))
         (heading (org-get-heading t t t t))
         (title (if (string-match "\\(.*\\) #[0-9]+$" heading)
                    (match-string 1 heading)
                  heading))
         (remote-title (when remote-data (alist-get 'title remote-data)))
         (deadline-str (org-entry-get (point) "DEADLINE"))
         (milestone (org-entry-get (point) "MILESTONE"))
         (assignees-str (org-entry-get (point) "ASSIGNEES"))
         (local-tags (org-get-tags nil t))
         (local-closed (or (string= todo-state org-github-closed-state)
                           (string= todo-state org-github-pr-closed-state)
                           (string= todo-state org-github-pr-merged-state)))
         (stored-closed (member (downcase (or local-state "open"))
                                '("closed" "merged"))))

    ;; --- 1. Push state (close/reopen) ---
    (cond
     ((and local-closed (not stored-closed))
      (org-github--run-gh-sync (list type "close" num "-R" repo))
      (org-set-property "STATE" "closed")
      (org-set-property "UPDATED_AT"
                        (format-time-string "[%Y-%m-%d %a %H:%M]"))
      (push "state→closed" changes))
     ((and (not local-closed) stored-closed)
      (org-github--run-gh-sync (list type "reopen" num "-R" repo))
      (org-set-property "STATE" "open")
      (when (org-entry-get (point) "CLOSED_AT")
        (org-delete-property "CLOSED_AT"))
      (org-set-property "UPDATED_AT"
                        (format-time-string "[%Y-%m-%d %a %H:%M]"))
      (push "state→open" changes)))

    ;; --- 2. Push title (skip if unchanged) ---
    (when (and title (not (string-empty-p title))
               (not (and remote-title (string= title remote-title))))
      (org-github--run-gh-sync
       (list type "edit" num "-R" repo "--title" title))
      (push "title" changes))

    ;; --- 3. Build single edit command for assignees + labels + milestone ---
    (let ((edit-args (list type "edit" num "-R" repo)))
      ;; Assignees: push all local as --add-assignee (idempotent)
      (when (and assignees-str (not (string-empty-p assignees-str)))
        (let ((logins (mapcar #'string-trim (split-string assignees-str ","))))
          (setq edit-args (append edit-args
                                  (list "--add-assignee" (string-join logins ","))))
          (push "assignees" changes)))
      ;; Labels: use GITHUB_LABELS (raw names from last pull) to avoid sanitization mismatch
      (let ((github-labels-str (org-entry-get (point) "GITHUB_LABELS")))
        (when (and github-labels-str (not (string-empty-p github-labels-str)))
          (setq edit-args (append edit-args
                                  (list "--add-label" github-labels-str)))
          (push "labels" changes)))
      ;; Milestone
      (if milestone
          (progn
            (setq edit-args (append edit-args (list "--milestone" milestone)))
            (push "milestone" changes))
        (setq edit-args (append edit-args (list "--remove-milestone"))))
      ;; Single gh call for all edit fields
      (org-github--run-gh-sync edit-args))

    ;; --- 4. Push deadline to GitHub Projects V2 ---
    (when deadline-str
      (org-github--push-deadline-at-point repo (string-to-number num) deadline-str)
      (push "deadline" changes))

    changes))

;;;###autoload
(defun org-github-sync-at-point ()
  "Bidirectional sync for the GitHub issue/PR at point.
Fetches the latest data from GitHub, compares UPDATED_AT timestamps,
and syncs in the appropriate direction:
- If GitHub is newer → pull (overwrite local with remote data).
- If local is newer  → push (post local changes to GitHub).
- If equal           → report already in sync.
With prefix arg \\[universal-argument], force pull from GitHub.
With double prefix \\[universal-argument] \\[universal-argument], force push to GitHub."
  (interactive)
  (let ((agenda-marker (when (derived-mode-p 'org-agenda-mode)
                         (or (org-get-at-bol 'org-hd-marker)
                             (org-get-at-bol 'org-marker)))))
    (if agenda-marker
        (with-current-buffer (marker-buffer agenda-marker)
          (save-excursion
            (goto-char (marker-position agenda-marker))
            (org-github-sync-at-point)))
      (save-excursion
        (org-back-to-heading t)
        (let ((issue-num (org-entry-get (point) "ISSUE_NUMBER"))
              (pr-num (org-entry-get (point) "PR_NUMBER"))
              (repo (org-entry-get (point) "REPO")))
          (if (and repo (or issue-num pr-num))
              (let* ((is-pr (not (null pr-num)))
                     (num-str (if is-pr pr-num issue-num))
                     (num (string-to-number num-str))
                     (type (if is-pr "pr" "issue"))
                     (prefix current-prefix-arg))

                (message "Syncing %s #%s from %s..." type num-str repo)

                ;; Fetch remote data
                (let* ((remote-data (if is-pr
                                        (org-github--fetch-single-pr repo num)
                                      (org-github--fetch-single-issue repo num)))
                       (remote-updated (alist-get 'updatedAt remote-data))
                       (local-updated (org-entry-get (point) "UPDATED_AT"))
                       (remote-time (org-github--parse-updated-at remote-updated))
                       (local-time (org-github--parse-updated-at local-updated))
                       (force-pull (equal prefix '(4)))
                       (force-push (equal prefix '(16))))

                  (cond
                   (force-pull
                    (org-github--pull-at-point repo num type remote-data)
                    (message "Pulled %s #%s from %s (forced)" type num-str repo))

                   (force-push
                    (let ((changes (org-github--push-at-point repo num-str type remote-data)))
                      (message "Pushed %s #%s to %s (forced): %s"
                               type num-str repo
                               (if changes
                                   (string-join (nreverse changes) ", ")
                                 "no changes"))))

                   ((time-less-p local-time remote-time)
                    ;; Remote is newer → pull
                    (org-github--pull-at-point repo num type remote-data)
                    (message "Pulled %s #%s from %s (remote was newer)" type num-str repo))

                   ((time-less-p remote-time local-time)
                    ;; Local is newer → push
                    (let ((changes (org-github--push-at-point repo num-str type remote-data)))
                      (message "Pushed %s #%s to %s (local was newer): %s"
                               type num-str repo
                               (if changes
                                   (string-join (nreverse changes) ", ")
                                 "no changes"))))

                   (t
                    ;; Timestamps equal — still pull to ensure metadata is fresh
                    (org-github--pull-at-point repo num type remote-data)
                    (message "%s #%s in %s is in sync" type num-str repo)))))

            (message "Not on a GitHub issue/PR")))))))

;;;###autoload
(defun org-github-pull-at-point ()
  "Pull the latest GitHub data for the issue/PR at point.
Overwrites local org heading with remote state, metadata, and timestamps."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (org-github-sync-at-point)))

;;;###autoload
(defun org-github-push-at-point ()
  "Push local org heading state to GitHub for the issue/PR at point.
Posts local state, title, assignees, labels, milestone, and deadline."
  (interactive)
  (let ((current-prefix-arg '(16)))
    (org-github-sync-at-point)))

(defun org-github--push-deadline-at-point (repo number deadline-str)
  "Push DEADLINE-STR to GitHub Projects V2 for issue NUMBER in REPO.
Requires a project mapping in `org-github-repo-project-alist'."
  (let ((project-config (cdr (assoc repo org-github-repo-project-alist))))
    (when project-config
      (let* ((owner (car project-config))
             (project-num (cdr project-config))
             ;; Need project/field/item IDs from GraphQL (metadata, not issue content)
             (query (format "{
  user(login: \"%s\") {
    projectV2(number: %d) {
      id
      field(name: \"Deadline\") { ... on ProjectV2SingleSelectField { id } ... on ProjectV2Field { id } }
      items(first: 100) {
        nodes {
          id
          content { ... on Issue { number repository { nameWithOwner } } }
        }
      }
    }
  }
}" owner project-num))
             (json-output (org-github--run-gh-sync
                           (list "api" "graphql" "-f" (concat "query=" query))))
             (data (org-github--parse-json json-output))
             (project-data (alist-get 'projectV2
                                       (alist-get 'user
                                                   (alist-get 'data data))))
             (project-id (alist-get 'id project-data))
             (field-id (alist-get 'id (alist-get 'field project-data)))
             (items (alist-get 'nodes (alist-get 'items project-data)))
             (item-id (cl-loop for item in items
                               when (let ((content (alist-get 'content item)))
                                      (and (= (alist-get 'number content) number)
                                           (string= (alist-get 'nameWithOwner
                                                                (alist-get 'repository content))
                                                    repo)))
                               return (alist-get 'id item))))
        (when (and project-id field-id item-id)
          (let ((date (format-time-string "%Y-%m-%d"
                                          (org-time-string-to-time deadline-str))))
            (org-github--run-gh-sync
             (list "project" "item-edit"
                   "--id" item-id
                   "--project-id" project-id
                   "--field-id" field-id
                   "--date" date))))))))

;;;###autoload
(defun org-github-sync-issue-states (&optional repo)
  "Sync org-mode states with GitHub for all issues in REPO.
Updates existing issues and adds new ones.  When a project mapping
exists in `org-github-repo-project-alist', also syncs deadlines."
  (interactive)
  (let* ((repo (or repo (completing-read "Repository: " org-github-default-repos
                                      nil nil nil nil (car org-github-default-repos))))
         (issues (org-github--fetch-issues repo "all"))
         (deadlines (org-github--fetch-project-deadlines repo))
         (org-file (org-github--get-repo-file repo))
         (org-github--syncing t)
         (updated-count 0)
         (new-count 0))
    (message "Syncing issues from %s..." repo)
    (with-current-buffer (find-file-noselect org-file)
      (save-restriction
        (widen)
        (dolist (issue issues)
          (let* ((number (alist-get 'number issue))
                 (state (alist-get 'state issue))
                 (deadline (cdr (assq number deadlines))))
            (if (org-github--issue-exists-p repo number)
                (when (org-github--update-issue-state repo number state deadline issue)
                  (setq updated-count (1+ updated-count)))
              (goto-char (org-github--find-or-create-repo-heading repo))
              ;; Inject deadline into issue data for formatting
              (when deadline
                (push (cons 'deadline deadline) issue))
              (insert (org-github--issue-to-org issue repo))
              (setq new-count (1+ new-count)))))
        (save-buffer)))
    (message "Synced %s: %d new, %d updated" repo new-count updated-count)))

;;;###autoload
(defun org-github-sync-pr-states (&optional repo)
  "Sync org-mode states with GitHub for all PRs in REPO.
Updates existing PRs and adds new ones.  When a project mapping
exists in `org-github-repo-project-alist', also syncs deadlines."
  (interactive)
  (let* ((repo (or repo (completing-read "Repository: " org-github-default-repos
                                      nil nil nil nil (car org-github-default-repos))))
         (prs (org-github--fetch-prs repo "all"))
         (deadlines (org-github--fetch-project-deadlines repo))
         (org-file (org-github--get-repo-file repo))
         (org-github--syncing t)
         (updated-count 0)
         (new-count 0))
    (message "Syncing PRs from %s..." repo)
    (with-current-buffer (find-file-noselect org-file)
      (save-restriction
        (widen)
        (dolist (pr prs)
          (let* ((number (alist-get 'number pr))
                 (state (alist-get 'state pr))
                 (merged (alist-get 'mergedAt pr))
                 (deadline (cdr (assq number deadlines))))
            (if (org-github--pr-exists-p repo number)
                (when (org-github--update-pr-state repo number state merged pr deadline)
                  (setq updated-count (1+ updated-count)))
              (goto-char (org-github--find-or-create-repo-heading repo))
              (when deadline
                (push (cons 'deadline deadline) pr))
              (insert (org-github--pr-to-org pr repo))
              (setq new-count (1+ new-count)))))
        (save-buffer)))
    (message "Synced PRs from %s: %d new, %d updated" repo new-count updated-count)))

;;;###autoload
(defun org-github-full-sync (&optional repo)
  "Full sync: download new issues/PRs and update states of existing ones."
  (interactive)
  (let* ((repo (or repo (completing-read "Repository: " org-github-default-repos
                                          nil nil nil nil (car org-github-default-repos))))
         (frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])
         (idx 0)
         (label "")
         (timer nil))
    (cl-flet ((spinner-start (msg)
                (setq label msg idx 0)
                (when timer (cancel-timer timer))
                (setq timer
                      (run-with-timer
                       0 0.1
                       (lambda ()
                         (message "%s %s"
                                  (aref frames (mod idx (length frames)))
                                  label)
                         (setq idx (1+ idx))))))
              (spinner-stop ()
                (when timer (cancel-timer timer) (setq timer nil))))
      (spinner-start (format "Full sync for %s: fetching issues..." repo))
      (org-github-sync-issue-states repo)
      (spinner-start (format "Full sync for %s: fetching PRs..." repo))
      (org-github-sync-pr-states repo)
      (spinner-stop)
      (message "✅ Full sync complete for %s" repo))))

;;;###autoload
(defun org-github-download-issues (&optional repo)
  "Download open issues from REPO."
  (interactive)
  (let* ((repo (or repo (completing-read "Repository: " org-github-default-repos
                                      nil nil nil nil (car org-github-default-repos))))
         (issues (org-github--fetch-issues repo "open"))
         (org-file (org-github--get-repo-file repo))
         (new-count 0))
    (message "Fetching issues from %s to %s..." repo org-file)
    (with-current-buffer (find-file-noselect org-file)
      (org-github--find-or-create-repo-heading repo)
      (dolist (issue issues)
        (let ((number (alist-get 'number issue)))
          (unless (org-github--issue-exists-p repo number)
            (goto-char (org-github--find-or-create-repo-heading repo))
            (insert (org-github--issue-to-org issue repo))
            (setq new-count (1+ new-count)))))
      (save-buffer))
    (message "Downloaded %d new issues from %s" new-count repo)))

;;;###autoload
(defun org-github-download-prs (&optional repo)
  "Download open PRs from REPO."
  (interactive)
  (let* ((repo (or repo (completing-read "Repository: " org-github-default-repos
                                      nil nil nil nil (car org-github-default-repos))))
         (prs (org-github--fetch-prs repo "open"))
         (org-file (org-github--get-repo-file repo))
         (new-count 0))
    (message "Fetching PRs from %s to %s..." repo org-file)
    (with-current-buffer (find-file-noselect org-file)
      (org-github--find-or-create-repo-heading repo)
      (dolist (pr prs)
        (let ((number (alist-get 'number pr)))
          (unless (org-github--pr-exists-p repo number)
            (goto-char (org-github--find-or-create-repo-heading repo))
            (insert (org-github--pr-to-org pr repo))
            (setq new-count (1+ new-count)))))
      (save-buffer))
    (message "Downloaded %d new PRs from %s" new-count repo)))

;;;###autoload
(defun org-github-download-all (&optional repo)
  "Pull everything for REPO into the local org file.
Does four things in order so a single command leaves the local copy
fully current:
  1. Download any new issues that don't exist locally.
  2. Download any new PRs that don't exist locally.
  3. Sync state + metadata (assignees, labels, milestone, deadline,
     updated-at) for every existing issue.
  4. Same for every existing PR.
If you only want to add newly-created items without touching existing
ones, call `org-github-download-issues' / `-download-prs' directly."
  (interactive)
  (let ((repo (or repo (completing-read "Repository: " org-github-default-repos
                                         nil nil nil nil (car org-github-default-repos)))))
    (org-github-download-issues repo)
    (org-github-download-prs repo)
    (org-github-sync-issue-states repo)
    (org-github-sync-pr-states repo)))

;;;###autoload
(defun org-github-sync-repos ()
  "Sync all default repos (download new and update existing states)."
  (interactive)
  (if org-github-default-repos
      (dolist (repo org-github-default-repos)
        (org-github-full-sync repo))
    (message "No repos configured. Set org-github-default-repos.")))

;;; Time Tracking Commands

;;;###autoload
(defun org-github-clock-in ()
  "Clock in on current issue/PR heading."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((issue-num (org-entry-get (point) "ISSUE_NUMBER"))
          (pr-num (org-entry-get (point) "PR_NUMBER")))
      (if (or issue-num pr-num)
          (progn
            (org-clock-in)
            (message "Clocked in on %s #%s"
                     (if pr-num "PR" "Issue")
                     (or pr-num issue-num)))
        (message "Not on a GitHub issue/PR")))))

;;;###autoload
(defun org-github-clock-out-and-log ()
  "Clock out and optionally log time to GitHub as a comment."
  (interactive)
  (when org-clock-current-marker
    (save-excursion
      (goto-char org-clock-current-marker)
      (let* ((issue-num (org-entry-get (point) "ISSUE_NUMBER"))
             (pr-num (org-entry-get (point) "PR_NUMBER"))
             (repo (org-entry-get (point) "REPO"))
             (clocked-minutes (org-clock-get-clocked-time))
             (hours (/ clocked-minutes 60))
             (mins (mod clocked-minutes 60))
             (time-str (format "%dh %dm" hours mins)))
        (org-clock-out)
        (when (and org-github-log-to-github repo (or issue-num pr-num))
          (when (y-or-n-p (format "Log %s to GitHub #%s? " time-str (or pr-num issue-num)))
            (message "Logging time to GitHub...")
            (org-github--run-gh-async
             (list "issue" "comment" (or issue-num pr-num)
                   "-R" repo
                   "-b" (format "Time tracked: %s" time-str))
             (lambda (_output error)
               (if error
                   (message "Failed to log time: %s" error)
                 (message "Time logged to GitHub #%s" (or pr-num issue-num)))))))))))

;;; Browser/View Commands

;;;###autoload
(defun org-github-open-in-browser ()
  "Open current issue/PR in browser."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((url (org-entry-get (point) "URL")))
      (if url
          (browse-url url)
        (message "No URL found")))))

;;;###autoload
(defun org-github-view-issue ()
  "View issue/PR details in a buffer."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((issue-num (org-entry-get (point) "ISSUE_NUMBER"))
          (pr-num (org-entry-get (point) "PR_NUMBER"))
          (repo (org-entry-get (point) "REPO")))
      (if (and repo (or issue-num pr-num))
          (let* ((type (if pr-num "pr" "issue"))
                 (num (or pr-num issue-num)))
            (message "Loading...")
            (org-github--run-gh-async
             (list type "view" num "-R" repo)
             (lambda (output error)
               (if error
                   (message "Failed to fetch %s #%s: %s" type num error)
                 (with-current-buffer (get-buffer-create "*GitHub View*")
                   (erase-buffer)
                   (insert output)
                   (goto-char (point-min))
                   (view-mode 1))
                 (pop-to-buffer "*GitHub View*")))))
        (message "Not on a GitHub issue/PR")))))

;;; Issue Management Commands

;;;###autoload
(defun org-github-close-issue ()
  "Close issue/PR at point on GitHub."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((issue-num (org-entry-get (point) "ISSUE_NUMBER"))
          (pr-num (org-entry-get (point) "PR_NUMBER"))
          (repo (org-entry-get (point) "REPO")))
      (if (and repo (or issue-num pr-num))
          (let* ((type (if pr-num "pr" "issue"))
                 (num (or pr-num issue-num)))
            (when (y-or-n-p (format "Close %s #%s? " type num))
              (let ((marker (point-marker)))
                (org-github--run-gh-async
                 (list type "close" num "-R" repo)
                 (lambda (_output error)
                   (if error
                       (message "Failed to close %s #%s: %s" type num error)
                     (when (buffer-live-p (marker-buffer marker))
                       (with-current-buffer (marker-buffer marker)
                         (goto-char (marker-position marker))
                         (let ((org-github--syncing t))
                           (org-todo org-github-closed-state)
                           (org-set-property "STATE" "closed")
                           (org-set-property "CLOSED_AT"
                                             (format-time-string "[%Y-%m-%d %a %H:%M]"))
                           (org-set-property "UPDATED_AT"
                                             (format-time-string "[%Y-%m-%d %a %H:%M]"))))
                       (message "Closed %s #%s" type num))))))))
        (message "Not on a GitHub issue/PR")))))

;;;###autoload
(defun org-github-add-comment ()
  "Add comment to issue/PR at point."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((issue-num (org-entry-get (point) "ISSUE_NUMBER"))
          (pr-num (org-entry-get (point) "PR_NUMBER"))
          (repo (org-entry-get (point) "REPO")))
      (if (and repo (or issue-num pr-num))
          (let* ((num (or pr-num issue-num))
                 (comment (read-string "Comment: ")))
            (when (not (string-empty-p comment))
              (org-github--run-gh-async
               (list "issue" "comment" num "-R" repo "-b" comment)
               (lambda (_output error)
                 (if error
                     (message "Failed to add comment to #%s: %s" num error)
                   (message "Comment added to #%s" num))))))
        (message "Not on a GitHub issue/PR")))))

;;;###autoload
(defun org-github-create-issue ()
  "Create GitHub issue from current Org heading."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let* ((title (org-get-heading t t t t))
           (body (org-get-entry))
           (repo (completing-read "Repository: " org-github-default-repos
                            nil nil nil nil (car org-github-default-repos))))
      (when (y-or-n-p (format "Create issue '%s' in %s? " title repo))
        (let ((marker (point-marker)))
          (org-github--run-gh-async
           (list "issue" "create" "-R" repo "-t" title "-b" (or body ""))
           (lambda (output error)
             (if error
                 (message "Failed to create issue: %s" error)
               (when (string-match "https://github.com/[^[:space:]]+" output)
                 (let ((url (match-string 0 output)))
                   (when (buffer-live-p (marker-buffer marker))
                     (with-current-buffer (marker-buffer marker)
                       (goto-char (marker-position marker))
                       (org-set-property "URL" url)
                       (when (string-match "/issues/\\([0-9]+\\)" url)
                         (org-set-property "ISSUE_NUMBER" (match-string 1 url)))
                       (org-set-property "REPO" repo)))
                   (message "Created: %s" url)))))))))))

;;; Git Analysis Commands

(defun org-github--extract-issue-from-commit (msg)
  "Extract issue number from commit MSG."
  (when (string-match "#\\([0-9]+\\)" msg)
    (string-to-number (match-string 1 msg))))

;;;###autoload
(defun org-github-extract-time-from-git (&optional repo-path)
  "Extract time from git history in REPO-PATH."
  (interactive "DRepository path: ")
  (let* ((default-directory (expand-file-name (or repo-path default-directory)))
         (log-output (shell-command-to-string
                      "git log --format='%H|%ai|%s' --since='1 year ago' 2>/dev/null"))
         (commits (split-string log-output "\n" t))
         (repo-name (string-trim
                     (shell-command-to-string
                      "git remote get-url origin 2>/dev/null | sed 's/.*github.com[:/]//;s/.git$//'"))))
    (message "Analyzing %d commits from %s..." (length commits) repo-name)
    (let ((issue-count 0))
      (dolist (commit commits)
        (when (string-match "#\\([0-9]+\\)" commit)
          (setq issue-count (1+ issue-count))))
      (message "Found %d commits referencing issues" issue-count))))

;;;###autoload
(defun org-github-estimate-time-from-commits (&optional repo-path)
  "Estimate time spent on issues from commits in REPO-PATH."
  (interactive "DRepository path: ")
  (let* ((default-directory (expand-file-name (or repo-path default-directory)))
         (log-output (shell-command-to-string
                      "git log --format='%H|%ai|%s' --since='30 days ago' 2>/dev/null"))
         (commits (nreverse (split-string log-output "\n" t)))
         (issue-work (make-hash-table :test 'equal))
         (prev-time nil)
         (repo-name (string-trim
                     (shell-command-to-string
                      "git remote get-url origin 2>/dev/null | sed 's/.*github.com[:/]//;s/.git$//'"))))
    (dolist (commit commits)
      (when (string-match "\\([^|]+\\)|\\([^|]+\\)|\\(.*\\)" commit)
        (let* ((date-str (match-string 2 commit))
               (msg (match-string 3 commit))
               (issue-num (org-github--extract-issue-from-commit msg))
               (commit-time (date-to-time date-str)))
          (when issue-num
            (let* ((key (format "%d" issue-num))
                   (existing (gethash key issue-work 0))
                   (duration (if prev-time
                                 (let ((gap (/ (float-time (time-subtract commit-time prev-time)) 3600)))
                                   (if (< gap 4) (* gap 60) 30))
                               30)))
              (puthash key (+ existing duration) issue-work))
            (setq prev-time commit-time)))))
    (with-current-buffer (get-buffer-create "*GitHub Time Summary*")
      (erase-buffer)
      (insert (format "# Time Estimate for %s\n\n" repo-name))
      (insert "| Issue | Time |\n|-------|------|\n")
      (maphash (lambda (k v)
                 (insert (format "| #%s | %dh %dm |\n" k (/ (truncate v) 60) (mod (truncate v) 60))))
               issue-work))
    (pop-to-buffer "*GitHub Time Summary*")))

;;; Bidirectional Sync (Org → GitHub)

(defvar org-github--syncing nil
  "Non-nil when a GitHub→Org sync is in progress.
Suppresses the todo-state-change hook to prevent feedback loops.")

(defun org-github--on-todo-state-change ()
  "Hook for `org-after-todo-state-change-hook'.
When a GitHub-linked heading is marked DONE, close the issue on
GitHub and prompt for an optional closing comment.
Suppressed during sync operations."
  (unless org-github--syncing
    (when-let* ((repo (org-entry-get (point) "REPO"))
                (issue-num (org-entry-get (point) "ISSUE_NUMBER"))
                (new-state org-state))
      (cond
       ;; Marked as DONE → close on GitHub
       ((string= new-state org-github-closed-state)
        (let* ((comment (read-string
                         (format "Closing note for #%s (empty to skip): " issue-num)))
               (marker (point-marker))
               (calls (append
                       (unless (string-empty-p comment)
                         (list (list "issue" "comment" issue-num "-R" repo "-b" comment)))
                       (list (list "issue" "close" issue-num "-R" repo)))))
          (condition-case err
              (org-github--run-calls-async
               calls
               (lambda (error)
                 (if error
                     (message "Failed to close issue #%s: %s" issue-num error)
                   (when (buffer-live-p (marker-buffer marker))
                     (with-current-buffer (marker-buffer marker)
                       (goto-char (marker-position marker))
                       (let ((org-github--syncing t))
                         (org-set-property "STATE" "CLOSED")
                         (org-set-property "CLOSED_AT"
                                           (format-time-string "[%Y-%m-%d %a %H:%M]"))
                         (org-set-property "UPDATED_AT"
                                           (format-time-string "[%Y-%m-%d %a %H:%M]")))))
                   (message "Closed #%s on GitHub%s" issue-num
                            (if (string-empty-p comment) "" " (with comment)")))))
            (error (message "Failed to close issue: %s" (error-message-string err))))))
       ;; Reopened (back to TODO/NEXT from DONE) → reopen on GitHub
       ((and (member new-state (cons org-github-issue-todo-state
                                      org-github-open-substates))
             (string= (downcase (or (org-entry-get (point) "STATE") "")) "closed"))
        (let ((marker (point-marker)))
          (condition-case err
              (org-github--run-gh-async
               (list "issue" "reopen" issue-num "-R" repo)
               (lambda (_output error)
                 (if error
                     (message "Failed to reopen issue #%s: %s" issue-num error)
                   (when (buffer-live-p (marker-buffer marker))
                     (with-current-buffer (marker-buffer marker)
                       (goto-char (marker-position marker))
                       (let ((org-github--syncing t))
                         (org-set-property "STATE" "OPEN")
                         (org-delete-property "CLOSED_AT")
                         (org-set-property "UPDATED_AT"
                                           (format-time-string "[%Y-%m-%d %a %H:%M]")))))
                   (message "Reopened #%s on GitHub" issue-num))))
            (error (message "Failed to reopen issue: %s" (error-message-string err))))))))))

;;;###autoload
(define-minor-mode org-github-mode
  "Global minor mode enabling bidirectional Org-to-GitHub state sync.
When enabled, changing a TODO state on a GitHub-linked heading
will update the issue/PR on GitHub accordingly."
  :global t
  :lighter " OGH"
  (if org-github-mode
      (add-hook 'org-after-todo-state-change-hook #'org-github--on-todo-state-change)
    (remove-hook 'org-after-todo-state-change-hook #'org-github--on-todo-state-change)))

;;;###autoload
(defun org-github-diagnose-at-point ()
  "Compare the item at point with the live GitHub record.
Fetches the current issue/PR from GitHub and prints a side-by-side
report of each field (state, assignees, labels, milestone, deadline,
updated-at) so you can see where local and remote diverged.  Useful
when the dashboard shows an item in a \"No X\" block but GitHub says
otherwise."
  (interactive)
  (let* ((repo (org-entry-get (point) "REPO"))
         (iss (org-entry-get (point) "ISSUE_NUMBER"))
         (pr (org-entry-get (point) "PR_NUMBER"))
         (num-str (or iss pr)))
    (unless (and repo num-str)
      (user-error "Not on a GitHub item heading"))
    (let* ((num (string-to-number num-str))
           (is-pr (not (null pr)))
           (remote (if is-pr
                       (org-github--fetch-single-pr repo num)
                     (org-github--fetch-single-issue repo num)))
           (local-state (org-entry-get (point) "STATE"))
           (local-assignees (org-entry-get (point) "ASSIGNEES"))
           (local-milestone (org-entry-get (point) "MILESTONE"))
           (local-deadline (org-entry-get (point) "DEADLINE"))
           (local-updated (org-entry-get (point) "UPDATED_AT"))
           (local-tags (org-get-tags nil t))
           (remote-state (alist-get 'state remote))
           (remote-assignees (mapconcat (lambda (a) (alist-get 'login a))
                                        (alist-get 'assignees remote) ", "))
           (remote-milestone (alist-get 'title (alist-get 'milestone remote)))
           (remote-labels (mapcar (lambda (l) (alist-get 'name l))
                                  (alist-get 'labels remote)))
           (remote-updated (alist-get 'updatedAt remote))
           (fmt (lambda (label l r)
                  (let ((match (equal (or l "") (or r ""))))
                    (format "  %-14s %s\n  %-14s %s\n  %-14s %s\n\n"
                            (concat label ":")
                            (if match "(match)" "(DIFFERS)")
                            "  local" (or l "(unset)")
                            "  remote" (or r "(unset)"))))))
      (with-current-buffer (get-buffer-create "*org-github-diagnose*")
        (erase-buffer)
        (insert (format "=== %s %s#%d ===\n\n" repo (if is-pr "PR" "issue") num))
        (if (null remote)
            (insert "ERROR: could not fetch from GitHub (check gh CLI auth & network)\n")
          (insert (funcall fmt "state" local-state remote-state))
          (insert (funcall fmt "assignees" local-assignees
                           (if (string-empty-p remote-assignees) nil remote-assignees)))
          (insert (funcall fmt "milestone" local-milestone remote-milestone))
          (insert (funcall fmt "deadline" local-deadline nil))
          (insert (funcall fmt "updated_at" local-updated remote-updated))
          (insert (format "  local tags:  %s\n" (or local-tags "(none)")))
          (insert (format "  remote lbls: %s\n" (or remote-labels "(none)")))
          (insert "\nIf any row says DIFFERS, the local copy is stale.  Run\n")
          (insert "`M-x org-github-sync-issue-states' (or -pr-states) to refresh,\n")
          (insert "or `M-x org-github-pull-at-point' to pull this one item now.\n"))
        (goto-char (point-min)))
      (display-buffer "*org-github-diagnose*"))))

;;; Async Sync

(defun org-github--run-gh-async (args callback)
  "Run gh CLI with ARGS asynchronously.
CALLBACK receives (OUTPUT ERROR-STRING) — one will be nil on completion."
  (if (not (org-github--gh-available-p))
      (funcall callback nil "GitHub CLI (gh) not found. Install from https://cli.github.com/")
    (let ((buf (generate-new-buffer " *org-github-async*")))
      (make-process
       :name "org-github-gh"
       :buffer buf
       :command (cons "gh" args)
       :noquery t
       :sentinel
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (let* ((exit-code (process-exit-status proc))
                  (output (with-current-buffer (process-buffer proc)
                            (buffer-string))))
             (when (buffer-live-p (process-buffer proc))
               (kill-buffer (process-buffer proc)))
             (if (zerop exit-code)
                 (funcall callback output nil)
               (funcall callback nil output)))))))))

(defun org-github--run-calls-async (calls done-cb)
  "Execute CALLS (list of gh arg lists) sequentially and asynchronously.
DONE-CB is called with nil on success or an error string on the first failure."
  (if (null calls)
      (funcall done-cb nil)
    (org-github--run-gh-async
     (car calls)
     (lambda (_output error)
       (if error
           (funcall done-cb error)
         (org-github--run-calls-async (cdr calls) done-cb))))))

(defun org-github--push-deadline-async (repo number deadline-str callback)
  "Push DEADLINE-STR to GitHub Projects V2 for issue NUMBER in REPO asynchronously.
CALLBACK receives nil on success or an error string."
  (let ((project-config (cdr (assoc repo org-github-repo-project-alist))))
    (if (not project-config)
        (funcall callback nil)
      (let* ((owner (car project-config))
             (project-num (cdr project-config))
             (query (format "{
  user(login: \"%s\") {
    projectV2(number: %d) {
      id
      field(name: \"Deadline\") {
        ... on ProjectV2SingleSelectField { id }
        ... on ProjectV2Field { id }
      }
      items(first: 100) {
        nodes {
          id
          content {
            ... on Issue { number repository { nameWithOwner } }
          }
        }
      }
    }
  }
}" owner project-num)))
        (org-github--run-gh-async
         (list "api" "graphql" "-f" (concat "query=" query))
         (lambda (output error)
           (if error
               (funcall callback error)
             (let* ((data (org-github--parse-json output))
                    (project-data (alist-get 'projectV2
                                             (alist-get 'user
                                                        (alist-get 'data data))))
                    (project-id (alist-get 'id project-data))
                    (field-id (alist-get 'id (alist-get 'field project-data)))
                    (items (alist-get 'nodes (alist-get 'items project-data)))
                    (item-id (cl-loop for item in items
                                      when (let ((content (alist-get 'content item)))
                                             (and (= (alist-get 'number content) number)
                                                  (string= (alist-get 'nameWithOwner
                                                                       (alist-get 'repository content))
                                                           repo)))
                                      return (alist-get 'id item))))
               (if (not (and project-id field-id item-id))
                   (funcall callback nil)
                 (let ((date (format-time-string "%Y-%m-%d"
                                                 (org-time-string-to-time deadline-str))))
                   (org-github--run-gh-async
                    (list "project" "item-edit"
                          "--id" item-id
                          "--project-id" project-id
                          "--field-id" field-id
                          "--date" date)
                    (lambda (_out err) (funcall callback err)))))))))))))

(defun org-github--collect-push-ops (repo num-str type remote-data)
  "Collect push operations for the GitHub item heading at point.
Returns a plist:
  :calls   — list of gh arg lists to run sequentially
  :post-fn — function taking a DONE-CB; applies org property updates then
             optionally pushes deadline to GitHub asynchronously
  :changes — list of change description strings"
  (let* ((todo-state (org-get-todo-state))
         (local-state (org-entry-get (point) "STATE"))
         (heading (org-get-heading t t t t))
         (title (if (string-match "\\(.*\\) #[0-9]+$" heading)
                    (match-string 1 heading)
                  heading))
         (remote-title (when remote-data (alist-get 'title remote-data)))
         (deadline-str (org-entry-get (point) "DEADLINE"))
         (milestone (org-entry-get (point) "MILESTONE"))
         (assignees-str (org-entry-get (point) "ASSIGNEES"))
         (github-labels-str (org-entry-get (point) "GITHUB_LABELS"))
         (local-closed (or (string= todo-state org-github-closed-state)
                           (string= todo-state org-github-pr-closed-state)
                           (string= todo-state org-github-pr-merged-state)))
         (stored-closed (member (downcase (or local-state "open"))
                                '("closed" "merged")))
         (calls '())
         (changes '())
         (post-props '())
         (post-deadline-args nil)
         (pos-marker (point-marker)))

    ;; --- State change ---
    (cond
     ((and local-closed (not stored-closed))
      (push (list type "close" num-str "-R" repo) calls)
      (push "state→closed" changes)
      (push (cons "STATE" "closed") post-props)
      (push (cons "UPDATED_AT" (format-time-string "[%Y-%m-%d %a %H:%M]")) post-props))
     ((and (not local-closed) stored-closed)
      (push (list type "reopen" num-str "-R" repo) calls)
      (push "state→open" changes)
      (push (cons "STATE" "open") post-props)
      (push (cons "UPDATED_AT" (format-time-string "[%Y-%m-%d %a %H:%M]")) post-props)
      (when (org-entry-get (point) "CLOSED_AT")
        (push (cons "CLOSED_AT" :delete) post-props))))

    ;; --- Metadata: title + assignees + labels + milestone (one combined call) ---
    (let ((edit-args (list type "edit" num-str "-R" repo)))
      (when (and title (not (string-empty-p title))
                 (not (and remote-title (string= title remote-title))))
        (setq edit-args (append edit-args (list "--title" title)))
        (push "title" changes))
      (when (and assignees-str (not (string-empty-p assignees-str)))
        (setq edit-args (append edit-args
                                (list "--add-assignee"
                                      (string-join (mapcar #'string-trim
                                                           (split-string assignees-str ","))
                                                   ","))))
        (push "assignees" changes))
      (when (and github-labels-str (not (string-empty-p github-labels-str)))
        (setq edit-args (append edit-args (list "--add-label" github-labels-str)))
        (push "labels" changes))
      (if milestone
          (progn
            (setq edit-args (append edit-args (list "--milestone" milestone)))
            (push "milestone" changes))
        (setq edit-args (append edit-args (list "--remove-milestone"))))
      (push edit-args calls))

    ;; --- Deadline ---
    (when (and deadline-str (assoc repo org-github-repo-project-alist))
      (setq post-deadline-args (list repo (string-to-number num-str) deadline-str))
      (push "deadline" changes))

    (list :calls (nreverse calls)
          :post-fn
          (let ((props (nreverse post-props))
                (dl-args post-deadline-args))
            (lambda (done-cb)
              (when (and (markerp pos-marker)
                         (buffer-live-p (marker-buffer pos-marker)))
                (with-current-buffer (marker-buffer pos-marker)
                  (save-excursion
                    (goto-char (marker-position pos-marker))
                    (org-back-to-heading t)
                    (let ((org-github--syncing t))
                      (dolist (pair props)
                        (if (eq (cdr pair) :delete)
                            (org-delete-property (car pair))
                          (org-set-property (car pair) (cdr pair))))))))
              (if dl-args
                  (apply #'org-github--push-deadline-async
                         (append dl-args
                                 (list (lambda (err)
                                         (when err
                                           (message "org-github: deadline push failed: %s" err))
                                         (funcall done-cb nil)))))
                (funcall done-cb nil))))
          :changes (nreverse changes))))

;;;###autoload
(defun org-github-sync-at-point-async (&optional prefix done-callback)
  "Asynchronous bidirectional sync for the GitHub issue/PR at point.
PREFIX: \\[universal-argument] force-pull, \\[universal-argument] \\[universal-argument] force-push.
DONE-CALLBACK is called with nil on success or an error string on failure."
  (org-back-to-heading t)
  (let ((issue-num (org-entry-get (point) "ISSUE_NUMBER"))
        (pr-num (org-entry-get (point) "PR_NUMBER"))
        (repo (org-entry-get (point) "REPO")))
    (if (not (and repo (or issue-num pr-num)))
        (progn
          (message "Not on a GitHub issue/PR")
          (when done-callback (funcall done-callback nil)))
      (let* ((is-pr (not (null pr-num)))
             (num-str (if is-pr pr-num issue-num))
             (num (string-to-number num-str))
             (type (if is-pr "pr" "issue"))
             (force-pull (equal prefix '(4)))
             (force-push (equal prefix '(16)))
             (sync-key (cons repo num-str))
             (org-marker (point-marker))
             (fetch-args
              (if is-pr
                  (list "pr" "view" num-str "-R" repo
                        "--json" "number,title,body,state,createdAt,updatedAt,closedAt,mergedAt,labels,assignees,milestone,url,author,headRefName,baseRefName")
                (list "issue" "view" num-str "-R" repo
                      "--json" "number,title,body,state,createdAt,updatedAt,closedAt,labels,assignees,milestone,url,author"))))
        (if (gethash sync-key org-github--active-syncs)
            (progn
              (message "org-github: %s #%s is already syncing" type num-str)
              (when done-callback (funcall done-callback nil)))
          (puthash sync-key t org-github--active-syncs)
          (message "Syncing %s #%s from %s..." type num-str repo)
          (org-github--run-gh-async
           fetch-args
           (lambda (output error)
             (remhash sync-key org-github--active-syncs)
             (if error
                 (progn
                   (message "org-github: failed to fetch %s #%s: %s" type num-str error)
                   (when done-callback (funcall done-callback error)))
               (let* ((remote-data (org-github--parse-json output))
                      (remote-updated (alist-get 'updatedAt remote-data))
                      (local-updated
                       (when (and (markerp org-marker)
                                  (buffer-live-p (marker-buffer org-marker)))
                         (with-current-buffer (marker-buffer org-marker)
                           (save-excursion
                             (goto-char (marker-position org-marker))
                             (org-entry-get (point) "UPDATED_AT")))))
                      (remote-time (org-github--parse-updated-at remote-updated))
                      (local-time (org-github--parse-updated-at local-updated)))
                 (cond
                  ;; Pull: remote newer, force-pull, or timestamps equal
                  ((or force-pull
                       (time-less-p local-time remote-time)
                       (and (not force-push)
                            (not (time-less-p local-time remote-time))
                            (not (time-less-p remote-time local-time))))
                   (when (and (markerp org-marker)
                              (buffer-live-p (marker-buffer org-marker)))
                     (with-current-buffer (marker-buffer org-marker)
                       (save-excursion
                         (goto-char (marker-position org-marker))
                         (org-back-to-heading t)
                         (org-github--pull-at-point repo num type remote-data))))
                   (message (cond
                             (force-pull "Pulled %s #%s from %s (forced)")
                             ((time-less-p local-time remote-time)
                              "Pulled %s #%s from %s (remote was newer)")
                             (t "%s #%s in %s is in sync"))
                            type num-str repo)
                   (when done-callback (funcall done-callback nil)))

                  ;; Push: local newer or force-push
                  (t
                   (let ((ops
                          (when (and (markerp org-marker)
                                     (buffer-live-p (marker-buffer org-marker)))
                            (with-current-buffer (marker-buffer org-marker)
                              (save-excursion
                                (goto-char (marker-position org-marker))
                                (org-back-to-heading t)
                                (org-github--collect-push-ops
                                 repo num-str type remote-data))))))
                     (if (null ops)
                         (when done-callback (funcall done-callback nil))
                       (let ((calls (plist-get ops :calls))
                             (post-fn (plist-get ops :post-fn))
                             (changes (plist-get ops :changes)))
                         (org-github--run-calls-async
                          calls
                          (lambda (push-error)
                            (if push-error
                                (progn
                                  (message "org-github: push failed for %s #%s: %s"
                                           type num-str push-error)
                                  (when done-callback (funcall done-callback push-error)))
                              (funcall post-fn
                                       (lambda (_)
                                         (message "Pushed %s #%s to %s: %s"
                                                  type num-str repo
                                                  (if changes
                                                      (string-join (nreverse changes) ", ")
                                                    "no changes"))
                                         (when done-callback
                                           (funcall done-callback nil)))))))))))))))))))))

(provide 'org-github)

;;; org-github.el ends here
