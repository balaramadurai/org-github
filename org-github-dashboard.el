;;; org-github-dashboard.el --- Team dashboard for org-github -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Bala Ramadurai
;; Author: Bala Ramadurai <bala@balaramadurai.net>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (org "9.0") (org-ql "0.8") (org-super-agenda "1.2") (org-github "1.0.0"))
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

;; org-github-dashboard provides a team dashboard for GitHub issues and PRs
;; tracked by org-github.  It uses org-ql and org-super-agenda to display
;; per-assignee progress bars, filterable views by repo/assignee/status/period,
;; and inline sync of individual items.
;;
;; Usage:
;;   M-x org-github-dashboard       — open the dashboard
;;   /                               — filter by repos, assignees, status, period
;;   S                               — sync the item at point from GitHub
;;
;; Requirements:
;; - org-github, org-ql, org-super-agenda

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-agenda)
(require 'org-ql)
(require 'org-super-agenda)

;; org-github functions used at runtime (avoids hard require for package manager compatibility)
(declare-function org-github--run-gh-sync "org-github")
(declare-function org-github--parse-json "org-github")
(declare-function org-github--update-pr-state "org-github")
(declare-function org-github--update-issue-state "org-github")

;;; Custom Variables

(defgroup org-github-dashboard nil
  "Settings for the org-github team dashboard."
  :group 'org
  :prefix "org-github-dashboard-")

(defcustom org-github-dashboard-repos nil
  "List of repos to include in the GitHub team dashboard.
When nil, all repos are shown (no filtering)."
  :type '(repeat string)
  :group 'org-github-dashboard)

(defcustom org-github-dashboard-assignees nil
  "List of assignees to include in the GitHub team dashboard.
When nil, all assignees are shown (no filtering)."
  :type '(repeat string)
  :group 'org-github-dashboard)

(defcustom org-github-dashboard-status 'all
  "Status filter for the GitHub team dashboard.
Can be `all', `todo', or `done'."
  :type '(choice (const :tag "All" all)
                 (const :tag "Open only" todo)
                 (const :tag "Done only" done))
  :group 'org-github-dashboard)

(defcustom org-github-dashboard-hide-empty t
  "When non-nil, hide assignees with 0 items from the dashboard."
  :type 'boolean
  :group 'org-github-dashboard)

(defcustom org-github-dashboard-excluded-assignees nil
  "List of assignees to always exclude from the dashboard.
Useful for team members who have left the organization."
  :type '(repeat string)
  :group 'org-github-dashboard)

(defcustom org-github-dashboard-period nil
  "Time period filter for the GitHub team dashboard.
When non-nil, a cons (LABEL . DAYS) limiting items to those
updated within the last DAYS days.  nil means no time filter."
  :type '(choice (const :tag "All time" nil)
                 (cons :tag "Period" string integer))
  :group 'org-github-dashboard)

(defcustom org-github-dashboard-discord-webhook-url nil
  "Discord webhook URL for sending weekly summaries.
When nil, `org-github-dashboard-send-discord' will prompt for a URL."
  :type '(choice (const :tag "Not configured" nil) string)
  :group 'org-github-dashboard)

;;; org-ql Predicates

(with-eval-after-load 'org-ql
  (org-ql-defpred github-issue ()
    "A GitHub issue has ISSUE_NUMBER and REPO properties."
    :body (and (property "ISSUE_NUMBER") (property "REPO")))

  (org-ql-defpred github-pr ()
    "A GitHub PR has PR_NUMBER and REPO properties."
    :body (and (property "PR_NUMBER") (property "REPO")))

  (org-ql-defpred github-item ()
    "A GitHub issue or PR (has REPO and either ISSUE_NUMBER or PR_NUMBER)."
    :body (and (property "REPO")
               (or (property "ISSUE_NUMBER") (property "PR_NUMBER"))))

  (org-ql-defpred github-assignee (name)
    "A GitHub item assigned to NAME (substring match on ASSIGNEES property)."
    :body (and (or (property "ISSUE_NUMBER") (property "PR_NUMBER"))
               (property "ASSIGNEES")
               (org-entry-get (point) "ASSIGNEES")
               (string-match-p (regexp-quote name)
                               (org-entry-get (point) "ASSIGNEES")))))

;;; org-ql-block sort support (upstream workaround)
;; org-ql-block does not expose a :sort option natively.
;; This defvar + advice enables sorting within agenda blocks.

(defvar org-ql-block-sort nil
  "Sort order for `org-ql-block' agenda blocks.
Can be a symbol like `deadline', `date', `priority', `scheduled',
or a list of such symbols.  When non-nil, items are sorted before display.")

(with-eval-after-load 'org-ql
  (define-advice org-ql-search-block (:around (orig-fn query) add-sort)
    "Advise `org-ql-search-block' to support `org-ql-block-sort'."
    (if org-ql-block-sort
        (let (narrow-p old-beg old-end)
          (when-let* ((from (pcase org-agenda-restrict
                              ('nil (org-agenda-files nil 'ifmode))
                              (_ (prog1 org-agenda-restrict
                                   (with-current-buffer org-agenda-restrict
                                     (setf old-beg (point-min) old-end (point-max)
                                           narrow-p t)
                                     (narrow-to-region org-agenda-restrict-begin
                                                       org-agenda-restrict-end))))))
                      (items (org-ql-select from query
                               :action 'element-with-markers
                               :narrow narrow-p
                               :sort org-ql-block-sort)))
            (when narrow-p
              (with-current-buffer from
                (narrow-to-region old-beg old-end)))
            (org-agenda-prepare)
            (insert (org-add-props (or org-ql-block-header
                                       (org-ql-view--header-line-format
                                        :buffers-files from :query query))
                        nil 'face 'org-agenda-structure)
                    "\n")
            (->> items
                 (-map #'org-ql-view--format-element)
                 org-agenda-finalize-entries
                 insert)
            (insert "\n")))
      (funcall orig-fn query))))

;;; Internal Functions

(defun org-github-dashboard--collect-repos ()
  "Collect unique REPO property values from GitHub issues in agenda files."
  (let ((repos (make-hash-table :test 'equal)))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-min))
              (while (re-search-forward ":REPO:" nil t)
                (let ((val (org-entry-get (point) "REPO")))
                  (when (and val (not (string-empty-p val)))
                    (puthash val t repos)))))))))
    (sort (hash-table-keys repos) #'string<)))

(defun org-github-dashboard--collect-assignees ()
  "Collect unique assignees from all GitHub issues in agenda files.
Return a sorted list of GitHub username strings."
  (let ((assignees (make-hash-table :test 'equal)))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-min))
              (while (re-search-forward ":ASSIGNEES:" nil t)
                (let ((val (org-entry-get (point) "ASSIGNEES")))
                  (when val
                    (dolist (name (split-string val "," t " +"))
                      (puthash (string-trim name) t assignees))))))))))
    (sort (hash-table-keys assignees) #'string<)))

(defun org-github-dashboard--repo-query ()
  "Return an org-ql predicate fragment for the active repo filter.
Return nil when no filter is active (all repos shown)."
  (when org-github-dashboard-repos
    (if (= 1 (length org-github-dashboard-repos))
        `(property "REPO" ,(car org-github-dashboard-repos))
      `(or ,@(mapcar (lambda (repo) `(property "REPO" ,repo))
                     org-github-dashboard-repos)))))

(defun org-github-dashboard--date-from-ts (ts)
  "Extract YYYY-MM-DD date string from an inactive Org timestamp TS.
TS is like \"[2026-02-15 Sun 14:30]\".  Return nil if invalid."
  (when (and ts (>= (length ts) 11))
    (substring ts 1 11)))

(defun org-github-dashboard--filtered-issue-query (&rest extra)
  "Build a (github-item) query respecting the repo filter.
EXTRA predicates are ANDed in.  Nil elements in EXTRA are ignored.
CANCELLED items and excluded assignees are always filtered out."
  (let ((repo-pred (org-github-dashboard--repo-query))
        (parts (list '(github-item) '(not (todo "CANCELLED"))))
        (excluded org-github-dashboard-excluded-assignees))
    (when repo-pred (push repo-pred parts))
    (dolist (name excluded)
      (push `(not (github-assignee ,name)) parts))
    (dolist (e extra) (when e (push e parts)))
    (if (cdr parts) (cons 'and (nreverse parts)) (car parts))))

(defun org-github-dashboard--period-query ()
  "Return an org-ql predicate for the time period filter on done items.
Return nil when no period filter is active."
  (when org-github-dashboard-period
    (let ((cutoff (format-time-string
                   "%Y-%m-%d"
                   (time-subtract (current-time)
                                  (days-to-time (cdr org-github-dashboard-period))))))
      `(org-github-dashboard--after-cutoff-p ,cutoff))))

(defun org-github-dashboard--after-cutoff-p (cutoff)
  "Non-nil if entry at point was closed on or after CUTOFF date string."
  (let ((date (org-github-dashboard--date-from-ts
               (org-entry-get (point) "CLOSED_AT"))))
    (and date (not (string< date cutoff)))))

(defun org-github-dashboard--count-issues (assignee &optional done-p)
  "Count GitHub issues assigned to ASSIGNEE across agenda files.
If DONE-P is non-nil, count DONE issues; otherwise count open (todo) issues.
Respect repo filter and period filter (for done items only)."
  (length (org-ql-select (org-agenda-files)
            (org-github-dashboard--filtered-issue-query
             (if done-p '(done) '(todo))
             (when assignee `(github-assignee ,assignee))
             (when (and done-p org-github-dashboard-period)
               (org-github-dashboard--period-query)))
            :action 'element-with-markers)))

(defun org-github-dashboard--collect-all-stats (assignee &optional extra-pred)
  "Collect all stats for ASSIGNEE in a single org-ql query.
Return plist (:open-issues N :open-prs N :done-issues N :done-prs N).
EXTRA-PRED is an additional predicate (e.g. for unassigned filtering).
Respect repo filter.  Period filter is applied to done items in-elisp."
  (let* ((cutoff (when org-github-dashboard-period
                   (format-time-string
                    "%Y-%m-%d"
                    (time-subtract (current-time)
                                   (days-to-time (cdr org-github-dashboard-period))))))
         (items (org-ql-select (org-agenda-files)
                  (org-github-dashboard--filtered-issue-query
                   (when assignee `(github-assignee ,assignee))
                   extra-pred)
                  :action (lambda ()
                            (let* ((is-pr (org-entry-get (point) "PR_NUMBER"))
                                   (is-done (org-entry-is-done-p))
                                   (closed (org-entry-get (point) "CLOSED_AT")))
                              (list (if is-pr 'pr 'issue) is-done
                                    (org-github-dashboard--date-from-ts closed))))))
         (oi 0) (op 0) (di 0) (dp 0))
    (dolist (item items)
      (let ((type (nth 0 item))
            (done (nth 1 item))
            (date (nth 2 item)))
        (if done
            (when (or (null cutoff)
                      (and date (not (string< date cutoff))))
              (if (eq type 'pr) (cl-incf dp) (cl-incf di)))
          (if (eq type 'pr) (cl-incf op) (cl-incf oi)))))
    (list :open-issues oi :open-prs op :done-issues di :done-prs dp)))

(defun org-github-dashboard--progress-bar (issues prs total width)
  "Return a progress bar of WIDTH showing ISSUES and PRS out of TOTAL.
Issues are green, PRs are cyan, remaining is gray."
  (if (zerop total)
      (make-string width ?-)
    (let* ((issue-w (/ (* issues width) total))
           (pr-w (/ (* prs width) total))
           (empty (- width issue-w pr-w)))
      (concat (propertize (make-string issue-w ?█) 'face '(:foreground "green"))
              (propertize (make-string pr-w ?█) 'face '(:foreground "cyan"))
              (propertize (make-string empty ?░) 'face '(:foreground "gray"))))))

(defun org-github-dashboard--count-unassigned (assignees &optional done-p)
  "Count GitHub issues not assigned to any of ASSIGNEES.
If DONE-P is non-nil, count DONE issues; otherwise count open (todo) issues.
Respect repo filter and period filter (for done items only)."
  (let ((not-clauses (mapcar (lambda (name) `(not (github-assignee ,name))) assignees)))
    (length (org-ql-select (org-agenda-files)
              (org-github-dashboard--filtered-issue-query
               (if done-p '(done) '(todo))
               `(and ,@not-clauses)
               (when (and done-p org-github-dashboard-period)
                 (org-github-dashboard--period-query)))
              :action 'element-with-markers))))

(defun org-github-dashboard--build-blocks ()
  "Build org-ql blocks for all discovered GitHub assignees.
Each assignee gets open issues followed by done issues.
Respect assignee, status, and period filters."
  (let* ((all-assignees (seq-remove
                         (lambda (a) (member a org-github-dashboard-excluded-assignees))
                         (org-github-dashboard--collect-assignees)))
         (assignees (if org-github-dashboard-assignees
                       (seq-filter (lambda (a) (member a org-github-dashboard-assignees))
                                   all-assignees)
                     all-assignees))
         (not-clauses (mapcar (lambda (name) `(not (github-assignee ,name)))
                              all-assignees))
         (show-open (and (memq org-github-dashboard-status '(all todo))
                         (not org-github-dashboard-period)))
         (show-done (memq org-github-dashboard-status '(all done))))
    (append
     ;; Per-assignee blocks
     (mapcan
      (lambda (name)
        (let* ((s (org-github-dashboard--collect-all-stats name))
               (open-count (+ (plist-get s :open-issues) (plist-get s :open-prs)))
               (done-count (+ (plist-get s :done-issues) (plist-get s :done-prs))))
          (when (or (not org-github-dashboard-hide-empty)
                    (> (+ open-count done-count) 0))
          (append
           (when show-open
             (let ((query (org-github-dashboard--filtered-issue-query
                           '(todo) `(github-assignee ,name))))
               (list
                `(org-ql-block ',query
                               ((org-ql-block-header ,(format "👤 %s (%d open)" name open-count))
                                (org-ql-block-sort 'deadline)
                                (org-super-agenda-groups
                                 '((:name "🔴 Overdue" :deadline past)
                                   (:name "⏰ Due Soon" :deadline future)
                                   (:name "📝 No Deadline" :anything t))))))))
           (when show-done
             (let ((query (org-github-dashboard--filtered-issue-query
                           '(done) `(github-assignee ,name)
                           (when org-github-dashboard-period
                             (org-github-dashboard--period-query)))))
               (list
                `(org-ql-block ',query
                               ((org-ql-block-header ,(format "✅ %s (%d done)" name done-count))
                                (org-ql-block-sort 'deadline)
                                (org-super-agenda-groups
                                 '((:anything t))))))))))))
      assignees)
     ;; Unassigned block (only if not filtering by assignees)
     (unless org-github-dashboard-assignees
       (let* ((ua-s (org-github-dashboard--collect-all-stats nil `(and ,@not-clauses)))
              (ua-open (+ (plist-get ua-s :open-issues) (plist-get ua-s :open-prs)))
              (ua-done (+ (plist-get ua-s :done-issues) (plist-get ua-s :done-prs))))
         (append
          (when show-open
            (let ((query (org-github-dashboard--filtered-issue-query
                          '(todo) `(and ,@not-clauses))))
              (list
               `(org-ql-block ',query
                              ((org-ql-block-header ,(format "❓ Unassigned (%d open)" ua-open))
                               (org-ql-block-sort 'deadline)
                               (org-super-agenda-groups
                                '((:name "🔴 Overdue" :deadline past)
                                  (:name "⏰ Due Soon" :deadline future)
                                  (:name "📝 No Deadline" :anything t))))))))
          (when show-done
            (let ((query (org-github-dashboard--filtered-issue-query
                          '(done) `(and ,@not-clauses)
                          (when org-github-dashboard-period
                            (org-github-dashboard--period-query)))))
              (list
               `(org-ql-block ',query
                              ((org-ql-block-header ,(format "✅ Unassigned (%d done)" ua-done))
                               (org-ql-block-sort 'deadline)
                               (org-super-agenda-groups
                                '((:anything t))))))))))))))

(defun org-github-dashboard--fixup-done-dates ()
  "Post-process agenda: rewrite \"due Xd ago\" to \"done Xd ago\" on DONE items."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^  DONE .+?\\( due \\([0-9]+d ago\\) \\)" nil t)
        (replace-match " done \\2 " t nil nil 1)))))

(defun org-github-dashboard--insert-summary-header ()
  "Insert a pictorial summary dashboard at the top of the agenda buffer."
  (let* ((inhibit-read-only t)
         (all-assignees (seq-remove
                         (lambda (a) (member a org-github-dashboard-excluded-assignees))
                         (org-github-dashboard--collect-assignees)))
         (assignees (if org-github-dashboard-assignees
                       (seq-filter (lambda (a) (member a org-github-dashboard-assignees))
                                   all-assignees)
                     all-assignees))
         (bar-width 20)
         (stats (mapcar (lambda (name)
                          (let ((s (org-github-dashboard--collect-all-stats name)))
                            (list name
                                  (plist-get s :open-issues) (plist-get s :open-prs)
                                  (plist-get s :done-issues) (plist-get s :done-prs)
                                  (+ (plist-get s :open-issues) (plist-get s :open-prs)
                                     (plist-get s :done-issues) (plist-get s :done-prs)))))
                        assignees))
         (unassigned-stats
          (if org-github-dashboard-assignees
              '(:open-issues 0 :open-prs 0 :done-issues 0 :done-prs 0)
            (let ((not-clauses (mapcar (lambda (name) `(not (github-assignee ,name)))
                                       all-assignees)))
              (org-github-dashboard--collect-all-stats nil `(and ,@not-clauses)))))
         (unassigned-total (+ (plist-get unassigned-stats :open-issues)
                              (plist-get unassigned-stats :open-prs)
                              (plist-get unassigned-stats :done-issues)
                              (plist-get unassigned-stats :done-prs)))
         (all-stats (if org-github-dashboard-assignees
                       stats
                     (append stats (list (list "Unassigned"
                                               (plist-get unassigned-stats :open-issues)
                                               (plist-get unassigned-stats :open-prs)
                                               (plist-get unassigned-stats :done-issues)
                                               (plist-get unassigned-stats :done-prs)
                                               unassigned-total)))))
         (max-name (apply #'max (mapcar (lambda (s) (length (car s))) all-stats)))
         (total-open-issues (apply #'+ (mapcar (lambda (s) (nth 1 s)) all-stats)))
         (total-open-prs (apply #'+ (mapcar (lambda (s) (nth 2 s)) all-stats)))
         (total-done-issues (apply #'+ (mapcar (lambda (s) (nth 3 s)) all-stats)))
         (total-done-prs (apply #'+ (mapcar (lambda (s) (nth 4 s)) all-stats)))
         (total-open (+ total-open-issues total-open-prs))
         (total-done (+ total-done-issues total-done-prs))
         (total-all (+ total-open total-done)))
    (save-excursion
      (goto-char (point-min))
      ;; Title + active filters
      (let ((filters nil))
        (when org-github-dashboard-repos
          (push (format "repos: %s" (string-join org-github-dashboard-repos ", "))
                filters))
        (when org-github-dashboard-assignees
          (push (format "assignees: %s" (string-join org-github-dashboard-assignees ", "))
                filters))
        (unless (eq org-github-dashboard-status 'all)
          (push (format "status: %s" org-github-dashboard-status) filters))
        (when org-github-dashboard-period
          (push (format "period: %s" (car org-github-dashboard-period)) filters))
        (insert (propertize "GitHub Team Progress" 'face '(:height 1.3 :weight bold))
                (if filters
                    (propertize (format "  [%s]" (string-join (nreverse filters) " | "))
                                'face '(:foreground "orange" :slant italic))
                  "")
                "\n"))
      ;; Team summary line
      (insert (propertize (format "  Total: %d items  |  %d open  |  %d done  (%d%%)"
                                  total-all total-open total-done
                                  (if (zerop total-all) 0
                                    (/ (* total-done 100) total-all)))
                          'face 'org-agenda-structure)
              "\n")
      (insert "  " (org-github-dashboard--progress-bar
                     total-done-issues total-done-prs total-all 40)
              "\n")
      (insert "  "
              (propertize "█ Issues" 'face '(:foreground "green"))
              "  "
              (propertize "█ PRs" 'face '(:foreground "cyan"))
              "  "
              (propertize "░ Open" 'face '(:foreground "gray"))
              "\n\n")
      ;; Per-assignee bars
      (let ((col-bar 22)
            (col-pct 46)
            (col-count 52))
        (dolist (entry all-stats)
          (let* ((name (nth 0 entry))
                 (open-i (nth 1 entry))
                 (open-p (nth 2 entry))
                 (done-i (nth 3 entry))
                 (done-p (nth 4 entry))
                 (total (nth 5 entry))
                 (done (+ done-i done-p))
                 (open (+ open-i open-p))
                 (pct (if (zerop total) 0 (/ (* done 100) total)))
                 (label (if (string= name "Unassigned")
                            (propertize name 'face '(:foreground "orange"))
                          name)))
            (when (or (not org-github-dashboard-hide-empty)
                      (> total 0))
            (insert "  " label
                    (propertize " " 'display `(space :align-to ,col-bar))
                    (org-github-dashboard--progress-bar done-i done-p total bar-width)
                    (if org-github-dashboard-period
                        (concat
                         (propertize " " 'display `(space :align-to ,col-pct))
                         (format "(%d done)" done))
                      (concat
                       (propertize " " 'display `(space :align-to ,col-pct))
                       (format "%3d%%" pct)
                       (propertize " " 'display `(space :align-to ,col-count))
                       (format "(%d done, %d open)" done open)))
                    "\n")))))
      (insert "\n" (make-string 60 ?─) "\n\n"))))

(defun org-github-dashboard--run (&rest _)
  "Agenda function: build dynamic blocks and run as composite agenda."
  (let* ((blocks (org-github-dashboard--build-blocks)))
    (setq org-agenda-custom-commands
          (cons `("g!" "GitHub Team Dashboard (dynamic)" ,blocks)
                (assoc-delete-all "g!" org-agenda-custom-commands)))
    (org-agenda-run-series "GitHub Team Dashboard" (list blocks))
    (org-github-dashboard--insert-summary-header)
    (org-github-dashboard--fixup-done-dates)
    (local-set-key (kbd "/") #'org-github-dashboard-toggle-filter)
    (local-set-key (kbd "S") #'org-github-dashboard-sync-item)))

;;; Interactive Commands

;;;###autoload
(defun org-github-dashboard-toggle-filter ()
  "Interactively filter the GitHub dashboard by repos, assignees, or status."
  (interactive)
  (let ((dimension (completing-read "Filter by: "
                                    '("repos" "assignees" "status" "period" "clear all")
                                    nil t)))
    (pcase dimension
      ("repos"
       (let* ((all-repos (org-github-dashboard--collect-repos))
              (selected (completing-read-multiple
                         "Include repos (comma-separated, empty=all): "
                         all-repos nil nil
                         (when org-github-dashboard-repos
                           (string-join org-github-dashboard-repos ",")))))
         (setq org-github-dashboard-repos
               (if (or (null selected) (equal selected '(""))) nil selected))))
      ("assignees"
       (let* ((all-assignees (seq-remove
                              (lambda (a) (member a org-github-dashboard-excluded-assignees))
                              (org-github-dashboard--collect-assignees)))
              (selected (completing-read-multiple
                         "Include assignees (comma-separated, empty=all): "
                         all-assignees nil nil
                         (when org-github-dashboard-assignees
                           (string-join org-github-dashboard-assignees ",")))))
         (setq org-github-dashboard-assignees
               (if (or (null selected) (equal selected '(""))) nil selected))))
      ("status"
       (let ((choice (completing-read "Show: " '("all" "open only" "done only") nil t)))
         (setq org-github-dashboard-status
               (pcase choice
                 ("open only" 'todo)
                 ("done only" 'done)
                 (_ 'all)))))
      ("period"
       (let ((choice (completing-read "Show items from: "
                                      '("all time" "today" "this week" "last 7 days"
                                        "last 2 weeks" "this month" "last 30 days")
                                      nil t)))
         (setq org-github-dashboard-period
               (pcase choice
                 ("today" '("today" . 0))
                 ("this week"
                  (let ((dow (string-to-number (format-time-string "%u"))))
                    (cons "this week" (1- dow))))
                 ("last 7 days" '("last 7 days" . 7))
                 ("last 2 weeks" '("last 2 weeks" . 14))
                 ("this month"
                  (let ((dom (string-to-number (format-time-string "%d"))))
                    (cons "this month" (1- dom))))
                 ("last 30 days" '("last 30 days" . 30))
                 (_ nil)))))
      ("clear all"
       (setq org-github-dashboard-repos nil
             org-github-dashboard-assignees nil
             org-github-dashboard-status 'all
             org-github-dashboard-period nil)))
    (when (derived-mode-p 'org-agenda-mode)
      (org-github-dashboard))))

;;;###autoload
(defun org-github-dashboard-sync-item ()
  "Sync the issue or PR at point in the dashboard from GitHub."
  (interactive)
  (let* ((marker (or (org-get-at-bol 'org-hd-marker)
                     (org-agenda-error)))
         (buf (marker-buffer marker))
         (pos (marker-position marker))
         repo number is-pr)
    (with-current-buffer buf
      (save-excursion
        (goto-char pos)
        (org-back-to-heading t)
        (setq repo (org-entry-get (point) "REPO"))
        (setq number (or (org-entry-get (point) "PR_NUMBER")
                         (org-entry-get (point) "ISSUE_NUMBER")))
        (setq is-pr (not (null (org-entry-get (point) "PR_NUMBER"))))))
    (unless (and repo number)
      (user-error "No REPO/ISSUE_NUMBER/PR_NUMBER properties found at point"))
    (setq number (string-to-number number))
    (message "Syncing %s #%d from %s..." (if is-pr "PR" "issue") number repo)
    (let* ((json-fields (if is-pr
                            "number,state,updatedAt,closedAt,mergedAt,labels,assignees"
                          "number,state,updatedAt,closedAt,labels,assignees,milestone"))
           (gh-cmd (if is-pr "pr" "issue"))
           (output (org-github--run-gh-sync
                    (list gh-cmd "view" (number-to-string number)
                          "-R" repo "--json" json-fields)))
           (data (org-github--parse-json output)))
      (if is-pr
          (let ((state (alist-get 'state data))
                (merged (alist-get 'mergedAt data)))
            (org-github--update-pr-state repo number state merged data)
            (message "Synced PR #%d from %s: %s" number repo
                     (if merged "merged" state)))
        (let ((state (alist-get 'state data)))
          (org-github--update-issue-state repo number state nil data)
          (message "Synced issue #%d from %s: %s" number repo state))))
    (org-agenda-redo t)))

;;;###autoload
(defun org-github-dashboard ()
  "Show GitHub team dashboard with dynamically discovered assignees."
  (interactive)
  (org-agenda nil "gt")
  (delete-other-windows))

;;; Discord Integration

(defun org-github-dashboard--collect-discord-items (repos target-date)
  "Collect open GitHub items from REPOS due on or before TARGET-DATE.
TARGET-DATE is a \"YYYY-MM-DD\" string.  Returns an alist of
((ASSIGNEE . ((:title T :type issue|pr :number N
               :repo R :deadline D :overdue BOOL) ...)) ...).
Items without a DEADLINE are included under a \"No Deadline\" section
when they are open."
  (let* ((org-github-dashboard-repos repos)
         (today (format-time-string "%Y-%m-%d"))
         (query (org-github-dashboard--filtered-issue-query '(todo)))
         (items (org-ql-select (org-agenda-files) query
                  :action (lambda ()
                            (let* ((title (org-get-heading t t t t))
                                   (repo (org-entry-get (point) "REPO"))
                                   (pr-num (org-entry-get (point) "PR_NUMBER"))
                                   (iss-num (org-entry-get (point) "ISSUE_NUMBER"))
                                   (assignees-raw (org-entry-get (point) "ASSIGNEES"))
                                   (dl-raw (org-entry-get (point) "DEADLINE"))
                                   (dl-date (when dl-raw
                                              (org-github-dashboard--date-from-ts dl-raw))))
                              ;; Include if: has deadline <= target-date, OR has no deadline
                              (when (or (and dl-date (not (string< target-date dl-date)))
                                        (null dl-date))
                                (list :title title :repo repo
                                      :type (if pr-num 'pr 'issue)
                                      :number (string-to-number (or pr-num iss-num "0"))
                                      :assignees (when assignees-raw
                                                   (split-string assignees-raw "," t " +"))
                                      :deadline dl-date
                                      :overdue (and dl-date (string< dl-date today))))))))
         (by-assignee (make-hash-table :test 'equal)))
    (setq items (delq nil items))
    ;; Skip items with no assignees
    (dolist (item items)
      (let ((names (plist-get item :assignees)))
        (when names
          (dolist (name names)
            (push item (gethash name by-assignee))))))
    (let ((result nil))
      (maphash (lambda (k v) (push (cons k (nreverse v)) result)) by-assignee)
      (sort result (lambda (a b) (string< (car a) (car b)))))))

(defun org-github-dashboard--collect-discord-completed (repos since-date)
  "Collect GitHub items from REPOS closed on or after SINCE-DATE.
SINCE-DATE is a \"YYYY-MM-DD\" string.  Returns an alist of
((ASSIGNEE . ((:title T :type issue|pr :number N
               :repo R :closed-date D) ...)) ...)."
  (let* ((org-github-dashboard-repos repos)
         (query (org-github-dashboard--filtered-issue-query '(done)))
         (items (org-ql-select (org-agenda-files) query
                  :action (lambda ()
                            (let* ((title (org-get-heading t t t t))
                                   (repo (org-entry-get (point) "REPO"))
                                   (pr-num (org-entry-get (point) "PR_NUMBER"))
                                   (iss-num (org-entry-get (point) "ISSUE_NUMBER"))
                                   (assignees-raw (org-entry-get (point) "ASSIGNEES"))
                                   (closed-raw (org-entry-get (point) "CLOSED_AT"))
                                   (closed-date (org-github-dashboard--date-from-ts closed-raw)))
                              (when (and closed-date
                                         (not (string< closed-date since-date)))
                                (list :title title :repo repo
                                      :type (if pr-num 'pr 'issue)
                                      :number (string-to-number (or pr-num iss-num "0"))
                                      :assignees (when assignees-raw
                                                   (split-string assignees-raw "," t " +"))
                                      :closed-date closed-date))))))
         (by-assignee (make-hash-table :test 'equal)))
    (setq items (delq nil items))
    ;; Skip items with no assignees
    (dolist (item items)
      (let ((names (plist-get item :assignees)))
        (when names
          (dolist (name names)
            (push item (gethash name by-assignee))))))
    (let ((result nil))
      (maphash (lambda (k v) (push (cons k (nreverse v)) result)) by-assignee)
      (sort result (lambda (a b) (string< (car a) (car b)))))))

(defun org-github-dashboard--format-discord-message (repos &optional target-date since-date)
  "Format a Discord markdown summary for REPOS.
When TARGET-DATE (a \"YYYY-MM-DD\" string) is given, only include
open items due on or before that date, grouped by assignee with
overdue items flagged.  When SINCE-DATE is also given, append a
section of items completed since that date.  When both are nil,
show the all-open-items weekly summary."
  (if (null target-date)
      ;; Original all-open-items summary
      (let* ((org-github-dashboard-repos repos)
             (all-assignees (seq-remove
                             (lambda (a) (member a org-github-dashboard-excluded-assignees))
                             (org-github-dashboard--collect-assignees)))
             (assignees (if org-github-dashboard-assignees
                           (seq-filter (lambda (a) (member a org-github-dashboard-assignees))
                                       all-assignees)
                         all-assignees))
             (mon (format-time-string "%b %d"))
             (sun (format-time-string "%b %d"
                                      (time-add (current-time) (days-to-time 6))))
             (lines (list (format "## Weekly GitHub Summary — %s to %s" mon sun) ""))
             (total-oi 0) (total-op 0))
        (dolist (name assignees)
          (let* ((s (org-github-dashboard--collect-all-stats name))
                 (oi (plist-get s :open-issues))
                 (op (plist-get s :open-prs)))
            (cl-incf total-oi oi)
            (cl-incf total-op op)
            (when (> (+ oi op) 0)
              (push (format "**%s** — %d open issue%s, %d open PR%s"
                            name oi (if (= oi 1) "" "s") op (if (= op 1) "" "s"))
                    lines))))
        (push "" lines)
        (push (format "**Team Total** — %d open issue%s, %d open PR%s"
                      total-oi (if (= total-oi 1) "" "s")
                      total-op (if (= total-op 1) "" "s"))
              lines)
        (push "" lines)
        (push (format "_Repos: %s_" (string-join (or repos '("all")) ", ")) lines)
        (string-join (nreverse lines) "\n"))
    ;; Date-filtered summary: due on or before target-date, with overdue flagged
    (let* ((by-assignee (org-github-dashboard--collect-discord-items repos target-date))
           (today (format-time-string "%Y-%m-%d"))
           (date-label (format-time-string "%a, %b %d"
                                           (date-to-time (concat target-date "T00:00:00Z"))))
           (lines (list (format "## GitHub Tasks Due by %s" date-label) ""))
           (total-due 0) (total-overdue 0))
      (dolist (entry by-assignee)
        (let* ((name (car entry))
               (items (cdr entry))
               (overdue (seq-filter (lambda (i) (plist-get i :overdue)) items))
               (due-on (seq-filter (lambda (i) (and (plist-get i :deadline)
                                                     (not (plist-get i :overdue)))) items))
               (no-deadline (seq-filter (lambda (i) (null (plist-get i :deadline))) items)))
          (cl-incf total-due (length items))
          (cl-incf total-overdue (length overdue))
          (push (format "### %s (%d item%s)" name (length items)
                        (if (= (length items) 1) "" "s"))
                lines)
          (when overdue
            (push (format "> :warning: **%d overdue**" (length overdue)) lines)
            (dolist (item overdue)
              (push (format "- ~~%s~~ %s #%d (was due %s)"
                            (plist-get item :title)
                            (if (eq (plist-get item :type) 'pr) "PR" "Issue")
                            (plist-get item :number)
                            (plist-get item :deadline))
                    lines)))
          (when due-on
            (dolist (item due-on)
              (push (format "- %s — %s #%d (due %s)"
                            (plist-get item :title)
                            (if (eq (plist-get item :type) 'pr) "PR" "Issue")
                            (plist-get item :number)
                            (plist-get item :deadline))
                    lines)))
          (when no-deadline
            (push (format "> %d with no deadline" (length no-deadline)) lines)
            (dolist (item no-deadline)
              (push (format "- %s — %s #%d"
                            (plist-get item :title)
                            (if (eq (plist-get item :type) 'pr) "PR" "Issue")
                            (plist-get item :number))
                    lines)))
          (push "" lines)))
      (when (zerop total-due)
        (push "No tasks due by this date. :tada:" lines)
        (push "" lines))
      (push (format "**Total: %d task%s due, %d overdue**"
                    total-due (if (= total-due 1) "" "s") total-overdue)
            lines)
      ;; Completed section
      (when since-date
        (let* ((by-assignee-done (org-github-dashboard--collect-discord-completed repos since-date))
               (since-label (format-time-string "%a, %b %d"
                                                (date-to-time (concat since-date "T00:00:00Z"))))
               (total-completed 0))
          (push "" lines)
          (push (format "---\n## Completed Since %s" since-label) lines)
          (push "" lines)
          (dolist (entry by-assignee-done)
            (let* ((name (car entry))
                   (items (cdr entry)))
              (cl-incf total-completed (length items))
              (push (format "### %s (%d completed)" name (length items)) lines)
              (dolist (item items)
                (push (format "- ~~%s~~ %s #%d (closed %s)"
                              (plist-get item :title)
                              (if (eq (plist-get item :type) 'pr) "PR" "Issue")
                              (plist-get item :number)
                              (plist-get item :closed-date))
                      lines))
              (push "" lines)))
          (if (zerop total-completed)
              (push "No items completed in this period." lines)
            (push (format "**Total: %d completed**" total-completed) lines))))
      (push "" lines)
      (push (format "_Repos: %s_" (string-join (or repos '("all")) ", ")) lines)
      (string-join (nreverse lines) "\n"))))

(defun org-github-dashboard--send-discord-webhook (message)
  "Send MESSAGE to the configured Discord webhook URL.
Splits into multiple messages if MESSAGE exceeds Discord's 2000
character limit.  Returns non-nil on success."
  (let ((url (or org-github-dashboard-discord-webhook-url
                 (user-error "Set `org-github-dashboard-discord-webhook-url' first")))
        (chunks (org-github-dashboard--split-message message 2000))
        (all-ok t))
    (dolist (chunk chunks)
      (let* ((url-request-method "POST")
             (url-request-extra-headers '(("Content-Type" . "application/json")))
             (payload (json-encode `((content . ,chunk))))
             (url-request-data (encode-coding-string payload 'utf-8))
             (buf (url-retrieve-synchronously url nil nil 30)))
        (if (null buf)
            (progn (message "Discord webhook: no response received") (setq all-ok nil))
          (unwind-protect
              (with-current-buffer buf
                (goto-char (point-min))
                (if (and (re-search-forward "HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
                         (member (match-string 1) '("200" "204")))
                    (message "Discord: sent %d chars" (length chunk))
                  (goto-char (point-min))
                  (message "Discord webhook failed: %s"
                           (buffer-substring (point-min) (min (point-max) 500)))
                  (setq all-ok nil)))
            (kill-buffer buf)))))
    (when all-ok
      (message "Discord message sent successfully (%d part%s)."
               (length chunks) (if (= (length chunks) 1) "" "s")))
    all-ok))

(defun org-github-dashboard--split-message (message max-len)
  "Split MESSAGE into chunks of at most MAX-LEN characters.
Splits on newline boundaries to avoid breaking lines."
  (if (<= (length message) max-len)
      (list message)
    (let ((lines (split-string message "\n"))
          (chunks nil)
          (current ""))
      (dolist (line lines)
        (let ((candidate (if (string-empty-p current)
                             line
                           (concat current "\n" line))))
          (if (<= (length candidate) max-len)
              (setq current candidate)
            (when (not (string-empty-p current))
              (push current chunks))
            (setq current line))))
      (when (not (string-empty-p current))
        (push current chunks))
      (nreverse chunks))))

;;;###autoload
(defun org-github-dashboard-send-discord (target-date since-date)
  "Send a GitHub summary to Discord.
TARGET-DATE is a \"YYYY-MM-DD\" string for the due-by cutoff.
SINCE-DATE is a \"YYYY-MM-DD\" string; completed items closed on or
after this date are included.  Interactively, both use the Org date
picker.  With prefix arg (C-u), sends the all-open-items weekly
summary instead.
Prompts for repos if `org-github-dashboard-repos' is not set.
Shows a preview buffer for confirmation before sending."
  (interactive
   (if current-prefix-arg
       (list nil nil)
     (list (org-read-date nil nil nil "Tasks due by: ")
           (org-read-date nil nil nil "Completed since: "))))
  (let* ((repos (or org-github-dashboard-repos
                    (let ((all (org-github-dashboard--collect-repos)))
                      (completing-read-multiple
                       "Repos (comma-separated): " all nil t))))
         (repo-list (if (listp repos) repos (list repos)))
         (msg (org-github-dashboard--format-discord-message repo-list target-date since-date)))
    (with-current-buffer (get-buffer-create "*Discord Preview*")
      (erase-buffer)
      (insert msg)
      (goto-char (point-min))
      (display-buffer (current-buffer)))
    (when (y-or-n-p "Send this message to Discord? ")
      (org-github-dashboard--send-discord-webhook msg))
    (when-let ((win (get-buffer-window "*Discord Preview*")))
      (delete-window win))
    (kill-buffer "*Discord Preview*")))

(provide 'org-github-dashboard)

;;; org-github-dashboard.el ends here
