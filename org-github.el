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

;;; Internal Functions

(defun org-github--gh-available-p ()
  "Check if gh CLI is available."
  (executable-find "gh"))

(defun org-github--run-gh-sync (args)
  "Run gh CLI with ARGS synchronously.
Returns the output string on success, signals error on failure."
  (unless (org-github--gh-available-p)
    (error "GitHub CLI (gh) not found. Install from https://cli.github.com/"))
  (with-temp-buffer
    (let ((exit-code (apply #'call-process "gh" nil t nil args)))
      (if (zerop exit-code)
          (buffer-string)
        (error "gh failed: %s" (buffer-string))))))

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
                             "--json" "number,title,body,state,createdAt,updatedAt,closedAt,mergedAt,labels,assignees,url,author,headRefName,baseRefName"
                             "--limit" "1000"))))
    (org-github--parse-json json-output)))

(defun org-github--fetch-project-deadlines (repo)
  "Fetch deadline field values from GitHub Projects V2 for REPO.
Returns an alist of (ISSUE-NUMBER . \"YYYY-MM-DD\") for issues that have
a Deadline field set.  Uses `org-github-repo-project-alist' to find
the project owner and number.  Paginates through all project items."
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
        (message "Fetched %d deadlines from project for %s" (length deadlines) repo)
        deadlines))))

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
         (labels (mapcar (lambda (l) (org-github--sanitize-tag (alist-get 'name l))) (alist-get 'labels pr)))
         (assignees (mapcar (lambda (a) (alist-get 'login a)) (alist-get 'assignees pr)))
         (todo-state (org-github--state-to-todo (if merged "merged" state) 'pr))
         (tags (concat ":PR:" (if labels (concat (string-join labels ":") ":") "")))
         (body-text (string-trim body)))
    (concat
     (format "*** %s PR#%d %s %s\n" todo-state number title tags)
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
              ;; Sync labels as tags
              (org-back-to-heading t)
              (let* ((labels (mapcar (lambda (l)
                                       (org-github--sanitize-tag (alist-get 'name l)))
                                     (alist-get 'labels issue)))
                     (new-tags (if labels (concat ":" (string-join labels ":") ":") nil))
                     (current-tags (org-get-tags nil t))
                     (current-tag-str (if current-tags
                                          (concat ":" (string-join current-tags ":") ":")
                                        nil)))
                (when (not (equal new-tags current-tag-str))
                  (if labels
                      (org-set-tags (string-join labels ":"))
                    (org-set-tags nil))
                  (setq changed t)))
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

(defun org-github--update-pr-state (repo number github-state merged &optional pr)
  "Update org-mode TODO state for PR NUMBER from REPO.
Uses GITHUB-STATE and MERGED timestamp to determine final state.
Optional PR is the full PR alist for updating timestamps."
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
              ;; Sync labels as tags
              (org-back-to-heading t)
              (let* ((labels (mapcar (lambda (l)
                                       (org-github--sanitize-tag (alist-get 'name l)))
                                     (alist-get 'labels pr)))
                     (new-tags (if labels (concat ":" (string-join labels ":") ":") nil))
                     (current-tags (org-get-tags nil t))
                     (current-tag-str (if current-tags
                                          (concat ":" (string-join current-tags ":") ":")
                                        nil)))
                (when (not (equal new-tags current-tag-str))
                  (if labels
                      (org-set-tags (string-join labels ":"))
                    (org-set-tags nil))
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
            changed))))))

;;; Interactive Commands

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
Updates existing PRs and adds new ones."
  (interactive)
  (let* ((repo (or repo (completing-read "Repository: " org-github-default-repos
                                      nil nil nil nil (car org-github-default-repos))))
         (prs (org-github--fetch-prs repo "all"))
         (org-file (org-github--get-repo-file repo))
         (org-github--syncing t)
         (updated-count 0)
         (new-count 0))
    (message "Syncing PRs from %s..." repo)
    (with-current-buffer (find-file-noselect org-file)
      (save-restriction
        (widen)
        (dolist (pr prs)
          (let ((number (alist-get 'number pr))
                (state (alist-get 'state pr))
                (merged (alist-get 'mergedAt pr)))
            (if (org-github--pr-exists-p repo number)
                (when (org-github--update-pr-state repo number state merged pr)
                  (setq updated-count (1+ updated-count)))
              (goto-char (org-github--find-or-create-repo-heading repo))
              (insert (org-github--pr-to-org pr repo))
              (setq new-count (1+ new-count)))))
        (save-buffer)))
    (message "Synced PRs from %s: %d new, %d updated" repo new-count updated-count)))

;;;###autoload
(defun org-github-full-sync (&optional repo)
  "Full sync: download new issues/PRs and update states of existing ones."
  (interactive)
  (let ((repo (or repo (completing-read "Repository: " org-github-default-repos
                                         nil nil nil nil (car org-github-default-repos)))))
    (org-github-sync-issue-states repo)
    (org-github-sync-pr-states repo)))

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
  "Download issues and PRs from REPO."
  (interactive)
  (let ((repo (or repo (completing-read "Repository: " org-github-default-repos
                                         nil nil nil nil (car org-github-default-repos)))))
    (org-github-download-issues repo)
    (org-github-download-prs repo)))

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
            (org-github--run-gh-sync
             (list "issue" "comment" (or issue-num pr-num)
                   "-R" repo
                   "-b" (format "Time tracked: %s" time-str)))))))))

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
                 (num (or pr-num issue-num))
                 (output (org-github--run-gh-sync
                          (list type "view" num "-R" repo))))
            (with-current-buffer (get-buffer-create "*GitHub View*")
              (erase-buffer)
              (insert output)
              (goto-char (point-min))
              (view-mode 1))
            (pop-to-buffer "*GitHub View*"))
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
              (org-github--run-gh-sync (list type "close" num "-R" repo))
              (org-todo org-github-closed-state)
              (org-set-property "STATE" "closed")
              (org-set-property "CLOSED_AT"
                                (format-time-string "[%Y-%m-%d %a %H:%M]"))
              (org-set-property "UPDATED_AT"
                                (format-time-string "[%Y-%m-%d %a %H:%M]"))
              (message "Closed %s #%s" type num)))
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
              (org-github--run-gh-sync
               (list "issue" "comment" num "-R" repo "-b" comment))
              (message "Comment added to #%s" num)))
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
        (let ((output (org-github--run-gh-sync
                       (list "issue" "create" "-R" repo "-t" title "-b" (or body "")))))
          (when (string-match "https://github.com/[^[:space:]]+" output)
            (let ((url (match-string 0 output)))
              (org-set-property "URL" url)
              (when (string-match "/issues/\\([0-9]+\\)" url)
                (org-set-property "ISSUE_NUMBER" (match-string 1 url)))
              (org-set-property "REPO" repo)
              (message "Created: %s" url))))))))

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
        (let ((comment (read-string
                        (format "Closing note for #%s (empty to skip): " issue-num))))
          (unless (string-empty-p comment)
            (condition-case err
                (org-github--run-gh-sync
                 (list "issue" "comment" issue-num "-R" repo "-b" comment))
              (error (message "Failed to add comment: %s" (error-message-string err)))))
          (condition-case err
              (progn
                (org-github--run-gh-sync
                 (list "issue" "close" issue-num "-R" repo))
                (org-set-property "STATE" "CLOSED")
                (org-set-property "CLOSED_AT"
                                  (format-time-string "[%Y-%m-%d %a %H:%M]"))
                (org-set-property "UPDATED_AT"
                                  (format-time-string "[%Y-%m-%d %a %H:%M]"))
                (message "Closed #%s on GitHub%s" issue-num
                         (if (string-empty-p comment) "" " (with comment)")))
            (error (message "Failed to close issue: %s" (error-message-string err))))))
       ;; Reopened (back to TODO/NEXT from DONE) → reopen on GitHub
       ((and (member new-state (cons org-github-issue-todo-state
                                      org-github-open-substates))
             (string= (downcase (or (org-entry-get (point) "STATE") "")) "closed"))
        (condition-case err
            (progn
              (org-github--run-gh-sync
               (list "issue" "reopen" issue-num "-R" repo))
              (org-set-property "STATE" "OPEN")
              (org-delete-property "CLOSED_AT")
              (org-set-property "UPDATED_AT"
                                (format-time-string "[%Y-%m-%d %a %H:%M]"))
              (message "Reopened #%s on GitHub" issue-num))
          (error (message "Failed to reopen issue: %s" (error-message-string err)))))))))

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

(provide 'org-github)

;;; org-github.el ends here
