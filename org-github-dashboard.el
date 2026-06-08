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
;; tracked by org-github.  It uses org-ql and org-super-agenda to display a
;; linear drill-down of attention items, per-assignee workloads and milestone
;; progress, with filterable views by repo/assignee/milestone/status/period and inline
;; sync of individual items.
;;
;; Sections are configurable via `org-github-dashboard-sections':
;;   summary     — team-total progress bar
;;   attention   — overdue, no-deadline, no-milestone, stale blocks
;;   assignees   — per-assignee drill-down (open + done)
;;   milestones  — per-milestone drill-down with progress bars
;;
;; Usage:
;;   M-x org-github-dashboard       — open the dashboard
;;   /                               — filter by repos, assignees, milestones, status, period
;;   V d / V w / V m / V a           — view: today / this week / this month / all
;;   TAB                             — fold / unfold the block at point
;;   <backtab> (S-TAB)               — cycle: fold all / unfold all
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
(declare-function org-github-sync-at-point-async "org-github")
(declare-function org-github--push-deadline-async "org-github")
(defvar org-github-repo-project-alist)

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

(defcustom org-github-dashboard-milestones nil
  "List of milestone names to include in the GitHub team dashboard.
When nil, all milestones are shown (no filtering)."
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

(defcustom org-github-dashboard-sections
  '(summary attention assignees milestones)
  "Top-level dashboard sections to display, in order.
Elements: `summary' (team-total bar), `attention' (problem-category
blocks: overdue, no-deadline, etc.), `assignees' (per-assignee
drill-down), `milestones' (per-milestone drill-down).  Remove an
element to hide that section."
  :type '(repeat (choice (const summary)
                         (const attention)
                         (const assignees)
                         (const milestones)))
  :group 'org-github-dashboard)

(defcustom org-github-dashboard-attention-categories
  '(overdue no-deadline no-milestone stale)
  "Problem categories shown in the attention section, in order.
Each element is one of `overdue', `no-deadline', `no-milestone',
`stale'.  Remove an element to hide that category."
  :type '(repeat (choice (const overdue)
                         (const no-deadline)
                         (const no-milestone)
                         (const stale)))
  :group 'org-github-dashboard)

(defcustom org-github-dashboard-stale-days 60
  "Open issues not updated in this many days are flagged as stale."
  :type 'integer
  :group 'org-github-dashboard)

(defcustom org-github-dashboard-excluded-milestones nil
  "List of milestone names to hide from the milestone section.
Useful for milestones that are closed or no longer active."
  :type '(repeat string)
  :group 'org-github-dashboard)

(defcustom org-github-dashboard-milestone-include-unassigned nil
  "When non-nil, include a \"(no milestone)\" block in the milestone section.
Defaults to nil because the attention section already shows a
no-milestone block when configured."
  :type 'boolean
  :group 'org-github-dashboard)

(defcustom org-github-dashboard-summary-show-assignee-bars nil
  "When non-nil, render per-assignee progress bars in the summary header.
Restores the pre-v2 layout where each assignee had a compact bar
below the team total.  The same information lives in the per-assignee
drill-down section, so this is off by default."
  :type 'boolean
  :group 'org-github-dashboard)

(defcustom org-github-dashboard-show-section-titles t
  "When non-nil, show small section titles above each group of blocks
\(Attention, Assignees, Milestones).  Helps visually distinguish
sections when several are shown back-to-back."
  :type 'boolean
  :group 'org-github-dashboard)

(defcustom org-github-dashboard-minimal-style nil
  "When non-nil, use a minimal rougier-inspired aesthetic.
Replaces emoji markers in block headers with typographic bullets
\(●, ·, ─) and applies lighter faces.  When nil, keeps the default
emoji-rich style."
  :type 'boolean
  :group 'org-github-dashboard)

(defcustom org-github-dashboard-discord-webhook-url nil
  "Discord webhook URL for sending weekly summaries.
When nil, `org-github-dashboard-send-discord' will prompt for a URL."
  :type '(choice (const :tag "Not configured" nil) string)
  :group 'org-github-dashboard)

(defcustom org-github-dashboard-discord-hide-no-deadline t
  "When non-nil, hide issues with no deadline from Discord summaries.
Set to nil to include them."
  :type 'boolean
  :group 'org-github-dashboard)

(defcustom org-github-dashboard-discord-hide-pr-linked t
  "When non-nil, hide issues that have an open PR linked via closing keywords.
PRs that reference an issue with Fixes/Closes/Resolves #N cause
that issue to be excluded from the Discord summary."
  :type 'boolean
  :group 'org-github-dashboard)

(defcustom org-github-dashboard-discord-webhook-alist nil
  "Alist mapping repository names to Discord webhook URLs.
Each entry is (REPO . URL).  When set, `org-github-dashboard-send-discord'
sends a separate message per repo to its designated webhook.
Repos not in this alist fall back to `org-github-dashboard-discord-webhook-url'."
  :type '(alist :key-type string :value-type string)
  :group 'org-github-dashboard)

(defcustom org-github-dashboard-email-motivations
  '("You've got this. One task at a time."
    "Small steps every day add up to big wins."
    "Progress, not perfection. Keep shipping."
    "Today is a great day to close a few issues."
    "Focus on what matters most — you'll get there.")
  "Fallback motivational lines for the email banner.
One is picked at random when no context-specific message applies.
See `org-github-dashboard--email-motivation'."
  :type '(repeat string)
  :group 'org-github-dashboard)

(defcustom org-github-dashboard-gantt-default-duration 7
  "Default task duration in days when CREATED_AT is missing.
Used as a fallback for Gantt chart start dates."
  :type 'integer
  :group 'org-github-dashboard)

(defcustom org-github-dashboard-report-title "GitHub Project Status Report"
  "Title for the investor report buffer."
  :type 'string
  :group 'org-github-dashboard)

(defcustom org-github-dashboard-report-include-gantt t
  "Whether to embed a Mermaid Gantt chart in the investor report."
  :type 'boolean
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
                               (org-entry-get (point) "ASSIGNEES"))))

  (org-ql-defpred github-stale (days)
    "A GitHub item whose UPDATED_AT is older than DAYS days ago."
    :body (let ((ts (org-entry-get (point) "UPDATED_AT")))
            (when (and ts (>= (length ts) 11))
              (string< (substring ts 1 11)
                       (format-time-string
                        "%Y-%m-%d"
                        (time-subtract (current-time)
                                       (days-to-time days))))))))

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
  "Build a (github-item) query respecting all active filters.
EXTRA predicates are ANDed in.  Nil elements in EXTRA are ignored.
Applies: repo filter, excluded assignees, positive assignee filter
\(`org-github-dashboard-assignees') and positive milestone filter
\(`org-github-dashboard-milestones')."
  (let ((repo-pred (org-github-dashboard--repo-query))
        (parts (list '(github-item) '(not (todo "CANCELLED"))))
        (excluded org-github-dashboard-excluded-assignees))
    (when repo-pred (push repo-pred parts))
    (dolist (name excluded)
      (push `(not (github-assignee ,name)) parts))
    (when org-github-dashboard-assignees
      (let ((preds (mapcar (lambda (a) `(github-assignee ,a))
                           org-github-dashboard-assignees)))
        (push (if (= 1 (length preds)) (car preds) `(or ,@preds))
              parts)))
    (when org-github-dashboard-milestones
      (let ((preds (mapcar (lambda (m) `(property "MILESTONE" ,m))
                           org-github-dashboard-milestones)))
        (push (if (= 1 (length preds)) (car preds) `(or ,@preds))
              parts)))
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

(defun org-github-dashboard--marker (kind)
  "Return the header prefix string for KIND.
KIND is one of `overdue', `no-deadline', `no-milestone', `stale',
`assignee', `unassigned', `milestone'.  When
`org-github-dashboard-minimal-style' is non-nil, returns a small
colored bullet; otherwise returns the emoji marker."
  (if org-github-dashboard-minimal-style
      (pcase kind
        ('overdue      (propertize "●" 'face '(:foreground "red3")))
        ('no-deadline  (propertize "●" 'face '(:foreground "orange3")))
        ('no-milestone (propertize "●" 'face '(:foreground "gold3")))
        ('stale        (propertize "●" 'face '(:foreground "gray50")))
        ('assignee     (propertize "·" 'face '(:foreground "gray60")))
        ('unassigned   (propertize "·" 'face '(:foreground "orange3")))
        ('milestone    (propertize "◇" 'face '(:foreground "steel blue"))))
    (pcase kind
      ('overdue      "🔴")
      ('no-deadline  "📅")
      ('no-milestone "🎯")
      ('stale        "🕸")
      ('assignee     "👤")
      ('unassigned   "❓")
      ('milestone    "🎯"))))

(defun org-github-dashboard--section-title (label)
  "Return a small-caps faded section-title string for LABEL."
  (propertize (format "\n── %s ─────────────────────────────\n\n"
                      (upcase label))
              'face '(:foreground "gray55" :weight light :slant italic)))

(defun org-github-dashboard--prepend-section-title (blocks label)
  "Return BLOCKS with LABEL prepended to the first block's header.
No-op when BLOCKS is empty or `org-github-dashboard-show-section-titles'
is nil."
  (if (and blocks org-github-dashboard-show-section-titles)
      (let* ((first (car blocks))
             (settings (nth 2 first))
             (new-settings
              (mapcar (lambda (s)
                        (if (eq (car-safe s) 'org-ql-block-header)
                            (list 'org-ql-block-header
                                  (concat (org-github-dashboard--section-title label)
                                          (cadr s)))
                          s))
                      settings)))
        (cons (list (nth 0 first) (nth 1 first) new-settings)
              (cdr blocks)))
    blocks))

(defun org-github-dashboard--collect-milestones ()
  "Collect unique MILESTONE property values from GitHub items in agenda files.
Return a sorted list of milestone name strings."
  (let ((milestones (make-hash-table :test 'equal)))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-min))
              (while (re-search-forward ":MILESTONE:" nil t)
                (let ((val (org-entry-get (point) "MILESTONE")))
                  (when (and val (not (string-empty-p val)))
                    (puthash val t milestones)))))))))
    (sort (hash-table-keys milestones) #'string<)))

(defun org-github-dashboard--count-query (query)
  "Return the number of entries matching QUERY across agenda files."
  (length (org-ql-select (org-agenda-files) query
            :action 'element-with-markers)))

(defun org-github-dashboard--build-attention-blocks ()
  "Build the needs-attention blocks according to `org-github-dashboard-attention-categories'.
Each category becomes a collapsible org-ql-block grouped by assignee
within.  Empty categories are skipped."
  (let ((blocks nil))
    (dolist (cat org-github-dashboard-attention-categories)
      (pcase cat
        ('overdue
         (let* ((query (org-github-dashboard--filtered-issue-query
                        '(todo) '(deadline :to -1)))
                (n (org-github-dashboard--count-query query)))
           (when (> n 0)
             (push `(org-ql-block ',query
                                  ((org-ql-block-header
                                    ,(format "%s Overdue (%d)"
                                             (org-github-dashboard--marker 'overdue) n))
                                   (org-ql-block-sort 'deadline)
                                   (org-super-agenda-groups
                                    '((:anything t)))))
                   blocks))))
        ('no-deadline
         (let* ((query (org-github-dashboard--filtered-issue-query
                        '(todo) '(not (deadline))))
                (n (org-github-dashboard--count-query query)))
           (when (> n 0)
             (push `(org-ql-block ',query
                                  ((org-ql-block-header
                                    ,(format "%s No deadline (%d)"
                                             (org-github-dashboard--marker 'no-deadline) n))
                                   (org-ql-block-sort 'deadline)
                                   (org-super-agenda-groups
                                    '((:anything t)))))
                   blocks))))
        ('no-milestone
         (let* ((query (org-github-dashboard--filtered-issue-query
                        '(todo) '(not (property "MILESTONE"))))
                (n (org-github-dashboard--count-query query)))
           (when (> n 0)
             (push `(org-ql-block ',query
                                  ((org-ql-block-header
                                    ,(format "%s No milestone (%d)"
                                             (org-github-dashboard--marker 'no-milestone) n))
                                   (org-ql-block-sort 'deadline)
                                   (org-super-agenda-groups
                                    '((:anything t)))))
                   blocks))))
        ('stale
         (let* ((days org-github-dashboard-stale-days)
                (query (org-github-dashboard--filtered-issue-query
                        '(todo) `(github-stale ,days)))
                (n (org-github-dashboard--count-query query)))
           (when (> n 0)
             (push `(org-ql-block ',query
                                  ((org-ql-block-header
                                    ,(format "%s Stale >%dd (%d)"
                                             (org-github-dashboard--marker 'stale) days n))
                                   (org-ql-block-sort 'deadline)
                                   (org-super-agenda-groups
                                    '((:anything t)))))
                   blocks))))))
    (nreverse blocks)))

(defun org-github-dashboard--assignee-attention-counts (name)
  "Return plist (:overdue N :no-deadline N) for assignee NAME."
  (list :overdue
        (org-github-dashboard--count-query
         (org-github-dashboard--filtered-issue-query
          '(todo) `(github-assignee ,name) '(deadline :to -1)))
        :no-deadline
        (org-github-dashboard--count-query
         (org-github-dashboard--filtered-issue-query
          '(todo) `(github-assignee ,name) '(not (deadline))))))

(defun org-github-dashboard--build-assignee-blocks ()
  "Build per-assignee drill-down blocks.
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
               (done-count (+ (plist-get s :done-issues) (plist-get s :done-prs)))
               (att (when show-open
                      (org-github-dashboard--assignee-attention-counts name)))
               (mk (org-github-dashboard--marker 'assignee))
               (open-header (if att
                                (format "%s %s (%d open: %d overdue, %d no-deadline)"
                                        mk name open-count
                                        (plist-get att :overdue)
                                        (plist-get att :no-deadline))
                              (format "%s %s (%d open)" mk name open-count))))
          (when (or (not org-github-dashboard-hide-empty)
                    (> (+ open-count done-count) 0))
          (append
           (when show-open
             (let ((query (org-github-dashboard--filtered-issue-query
                           '(todo) `(github-assignee ,name))))
               (list
                `(org-ql-block ',query
                               ((org-ql-block-header ,open-header)
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
                               ((org-ql-block-header
                                 ,(format "%s %s (%d done)"
                                          (if org-github-dashboard-minimal-style
                                              (propertize "✓" 'face '(:foreground "green4"))
                                            "✅")
                                          name done-count))
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
                              ((org-ql-block-header
                                ,(format "%s Unassigned (%d open)"
                                         (org-github-dashboard--marker 'unassigned) ua-open))
                               (org-ql-block-sort 'deadline)
                               (org-super-agenda-groups
                                '((:anything t))))))))
          (when show-done
            (let ((query (org-github-dashboard--filtered-issue-query
                          '(done) `(and ,@not-clauses)
                          (when org-github-dashboard-period
                            (org-github-dashboard--period-query)))))
              (list
               `(org-ql-block ',query
                              ((org-ql-block-header
                                ,(format "%s Unassigned (%d done)"
                                         (if org-github-dashboard-minimal-style
                                             (propertize "✓" 'face '(:foreground "green4"))
                                           "✅")
                                         ua-done))
                               (org-ql-block-sort 'deadline)
                               (org-super-agenda-groups
                                '((:anything t))))))))))))))

(defun org-github-dashboard--build-milestone-blocks ()
  "Build per-milestone drill-down blocks.
Each milestone gets one collapsible block whose header shows a
progress bar and counts.  Excluded milestones are skipped.  When
`org-github-dashboard-milestone-include-unassigned' is non-nil,
append a block for items with no milestone."
  (let* ((all (org-github-dashboard--collect-milestones))
         (milestones (seq-remove
                      (lambda (m) (member m org-github-dashboard-excluded-milestones))
                      all))
         (milestones (if org-github-dashboard-milestones
                         (seq-filter (lambda (m)
                                       (member m org-github-dashboard-milestones))
                                     milestones)
                       milestones))
         (blocks nil))
    (dolist (m milestones)
      (let* ((open-query (org-github-dashboard--filtered-issue-query
                          '(todo) `(property "MILESTONE" ,m)))
             (done-query (org-github-dashboard--filtered-issue-query
                          '(done) `(property "MILESTONE" ,m)))
             (all-query (org-github-dashboard--filtered-issue-query
                         `(property "MILESTONE" ,m)))
             (open-n (org-github-dashboard--count-query open-query))
             (done-n (org-github-dashboard--count-query done-query))
             (total (+ open-n done-n))
             (pct (if (zerop total) 0 (/ (* done-n 100) total)))
             (bar (org-github-dashboard--progress-bar done-n 0 total 15)))
        (when (> total 0)
          (push `(org-ql-block ',all-query
                               ((org-ql-block-header
                                 ,(format "%s %s  %s  %d%%  (%d/%d done)"
                                          (org-github-dashboard--marker 'milestone)
                                          m bar pct done-n total))
                                (org-ql-block-sort 'deadline)
                                (org-super-agenda-groups
                                 '((:anything t)))))
                blocks))))
    (when org-github-dashboard-milestone-include-unassigned
      (let* ((query (org-github-dashboard--filtered-issue-query
                     '(todo) '(not (property "MILESTONE"))))
             (n (org-github-dashboard--count-query query)))
        (when (> n 0)
          (push `(org-ql-block ',query
                               ((org-ql-block-header
                                 ,(format "%s (no milestone)  %d open"
                                          (org-github-dashboard--marker 'milestone) n))
                                (org-ql-block-sort 'deadline)
                                (org-super-agenda-groups
                                 '((:anything t)))))
                blocks))))
    (nreverse blocks)))

(defun org-github-dashboard--build-blocks ()
  "Build org-ql blocks for the configured dashboard sections.
Dispatches on `org-github-dashboard-sections'.  The `summary' section
is rendered by the header (not an org-ql-block) and is skipped here.
When `org-github-dashboard-show-section-titles' is non-nil, a small
section title is prepended to the first block of each section."
  (let ((blocks nil))
    (dolist (section org-github-dashboard-sections)
      (pcase section
        ('attention
         (setq blocks (append blocks
                              (org-github-dashboard--prepend-section-title
                               (org-github-dashboard--build-attention-blocks)
                               "Attention"))))
        ('assignees
         (setq blocks (append blocks
                              (org-github-dashboard--prepend-section-title
                               (org-github-dashboard--build-assignee-blocks)
                               "Assignees"))))
        ('milestones
         (setq blocks (append blocks
                              (org-github-dashboard--prepend-section-title
                               (org-github-dashboard--build-milestone-blocks)
                               "Milestones"))))))
    blocks))

(defun org-github-dashboard--fixup-done-dates ()
  "Post-process agenda: rewrite \"due Xd ago\" to \"done Xd ago\" on DONE items."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^  DONE .+?\\( due \\([0-9]+d ago\\) \\)" nil t)
        (replace-match " done \\2 " t nil nil 1)))))

(defun org-github-dashboard--restyle-section-titles ()
  "Re-apply the faded face to \"── SECTION ──\" lines.
Needed because `org-agenda-structure' overrides the face put on the
section-title substring when its containing block header is inserted."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "── [A-Z][A-Z]+ ─+" nil t)
        (add-face-text-property (match-beginning 0) (match-end 0)
                                '(:foreground "gray55" :weight light
                                              :slant italic :height 0.9)
                                nil)))))

(defun org-github-dashboard--insert-summary-header ()
  "Insert the team-total summary at the top of the agenda buffer.
Shows the title, active filters, total/open/done counts and the team
progress bar.  When `org-github-dashboard-summary-show-assignee-bars'
is non-nil, also renders compact per-assignee bars."
  (let* ((inhibit-read-only t)
         (all-assignees (seq-remove
                         (lambda (a) (member a org-github-dashboard-excluded-assignees))
                         (org-github-dashboard--collect-assignees)))
         (assignees (if org-github-dashboard-assignees
                       (seq-filter (lambda (a) (member a org-github-dashboard-assignees))
                                   all-assignees)
                     all-assignees))
         (named-stats (mapcar (lambda (name)
                                (cons name (org-github-dashboard--collect-all-stats name)))
                              assignees))
         (unassigned-stats
          (if org-github-dashboard-assignees
              '(:open-issues 0 :open-prs 0 :done-issues 0 :done-prs 0)
            (let ((not-clauses (mapcar (lambda (name) `(not (github-assignee ,name)))
                                       all-assignees)))
              (org-github-dashboard--collect-all-stats nil `(and ,@not-clauses)))))
         (all-named-stats (if org-github-dashboard-assignees
                              named-stats
                            (append named-stats
                                    (list (cons "Unassigned" unassigned-stats)))))
         (sum-key (lambda (k)
                    (apply #'+ (mapcar (lambda (s) (plist-get (cdr s) k))
                                       all-named-stats))))
         (total-open-issues (funcall sum-key :open-issues))
         (total-open-prs (funcall sum-key :open-prs))
         (total-done-issues (funcall sum-key :done-issues))
         (total-done-prs (funcall sum-key :done-prs))
         (total-open (+ total-open-issues total-open-prs))
         (total-done (+ total-done-issues total-done-prs))
         (total-all (+ total-open total-done)))
    (save-excursion
      (goto-char (point-min))
      (let ((filters nil))
        (when org-github-dashboard-repos
          (push (format "repos: %s" (string-join org-github-dashboard-repos ", "))
                filters))
        (when org-github-dashboard-milestones
          (push (format "milestones: %s" (string-join org-github-dashboard-milestones ", "))
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
              "\n")
      (when org-github-dashboard-summary-show-assignee-bars
        (insert "\n")
        (let ((col-bar 22)
              (col-pct 46)
              (col-count 52)
              (bar-width 20))
          (dolist (entry all-named-stats)
            (let* ((name (car entry))
                   (s (cdr entry))
                   (open-i (plist-get s :open-issues))
                   (open-p (plist-get s :open-prs))
                   (done-i (plist-get s :done-issues))
                   (done-p (plist-get s :done-prs))
                   (total (+ open-i open-p done-i done-p))
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
                        (org-github-dashboard--progress-bar
                         done-i done-p total bar-width)
                        (if org-github-dashboard-period
                            (concat
                             (propertize " " 'display `(space :align-to ,col-pct))
                             (format "(%d done)" done))
                          (concat
                           (propertize " " 'display `(space :align-to ,col-pct))
                           (format "%3d%%" pct)
                           (propertize " " 'display `(space :align-to ,col-count))
                           (format "(%d done, %d open)" done open)))
                        "\n"))))))
      (insert "\n" (make-string 60 ?─) "\n\n"))))

(defun org-github-dashboard--set-period (label days)
  "Set `org-github-dashboard-period' to (LABEL . DAYS) and refresh the dashboard.
Pass LABEL as nil to clear the period filter."
  (setq org-github-dashboard-period (when label (cons label days)))
  (org-github-dashboard))

(defun org-github-dashboard-view-day ()
  "Filter the dashboard to items from today only."
  (interactive)
  (org-github-dashboard--set-period "today" 0))

(defun org-github-dashboard-view-week ()
  "Filter the dashboard to items from the current ISO week."
  (interactive)
  (org-github-dashboard--set-period
   "this week" (1- (string-to-number (format-time-string "%u")))))

(defun org-github-dashboard-view-month ()
  "Filter the dashboard to items from the current calendar month."
  (interactive)
  (org-github-dashboard--set-period
   "this month" (1- (string-to-number (format-time-string "%d")))))

(defun org-github-dashboard-view-all ()
  "Clear the period filter on the dashboard."
  (interactive)
  (org-github-dashboard--set-period nil 0))

;;; Block folding

(defun org-github-dashboard--header-face-p (pos)
  "Non-nil when POS sits inside an `org-agenda-structure'-faced block header."
  (let ((face (get-text-property pos 'face)))
    (or (eq face 'org-agenda-structure)
        (and (listp face) (memq 'org-agenda-structure face)))))

(defun org-github-dashboard--on-header-line-p ()
  "Non-nil when the current line is a block header."
  (save-excursion
    (beginning-of-line)
    (or (org-github-dashboard--header-face-p (point))
        (let ((eol (line-end-position)))
          (and (< (point) eol)
               (org-github-dashboard--header-face-p (1+ (point))))))))

(defun org-github-dashboard--current-block-region ()
  "Return (BODY-START . BODY-END) for the block whose header is on the current line.
BODY-START is the position right after the header's newline.
BODY-END is the position of the next block header, or point-max."
  (save-excursion
    (beginning-of-line)
    (when (org-github-dashboard--on-header-line-p)
      (forward-line 1)
      (let ((body-start (point))
            body-end)
        (while (and (not body-end) (not (eobp)))
          (if (org-github-dashboard--header-face-p (point))
              (setq body-end (point))
            (forward-line 1)))
        (cons body-start (or body-end (point-max)))))))

(defun org-github-dashboard--block-fold-overlay (start end)
  "Return the existing fold overlay between START and END, or nil."
  (cl-find-if (lambda (ov) (overlay-get ov 'org-github-dashboard-fold))
              (overlays-in start end)))

(defun org-github-dashboard-toggle-fold ()
  "Toggle folding of the block at point.
If point is not on a block header line, fall back to
`org-agenda-goto' so TAB still navigates to the source item."
  (interactive)
  (if (not (org-github-dashboard--on-header-line-p))
      (call-interactively #'org-agenda-goto)
    (when-let ((region (org-github-dashboard--current-block-region)))
      (let* ((start (car region))
             (end (cdr region))
             (existing (org-github-dashboard--block-fold-overlay start end)))
        (if existing
            (delete-overlay existing)
          (when (> end start)
            (let ((ov (make-overlay start end nil t nil)))
              (overlay-put ov 'invisible t)
              (overlay-put ov 'org-github-dashboard-fold t)
              (overlay-put ov 'evaporate t))))))))

(defun org-github-dashboard--all-fold-overlays ()
  "Return all fold overlays currently active in the buffer."
  (cl-remove-if-not
   (lambda (ov) (overlay-get ov 'org-github-dashboard-fold))
   (overlays-in (point-min) (point-max))))

(defun org-github-dashboard-cycle-all ()
  "Fold all blocks if any are unfolded, otherwise unfold all."
  (interactive)
  (let ((overlays (org-github-dashboard--all-fold-overlays)))
    (if overlays
        (progn (mapc #'delete-overlay overlays)
               (message "org-github-dashboard: unfolded all"))
      (save-excursion
        (goto-char (point-min))
        (let ((count 0))
          (while (not (eobp))
            (when (org-github-dashboard--on-header-line-p)
              (org-github-dashboard-toggle-fold)
              (cl-incf count))
            (forward-line 1))
          (message "org-github-dashboard: folded %d block%s"
                   count (if (= count 1) "" "s")))))))

;;;###autoload
(defun org-github-dashboard-diagnose ()
  "Print what the dashboard will render.  For debugging empty sections."
  (interactive)
  (let* ((sections org-github-dashboard-sections)
         (att-cats org-github-dashboard-attention-categories)
         (period org-github-dashboard-period)
         (repos org-github-dashboard-repos)
         (status org-github-dashboard-status)
         (assignees (seq-remove
                     (lambda (a) (member a org-github-dashboard-excluded-assignees))
                     (org-github-dashboard--collect-assignees)))
         (milestones (seq-remove
                      (lambda (m) (member m org-github-dashboard-excluded-milestones))
                      (org-github-dashboard--collect-milestones)))
         (buf (get-buffer-create "*org-github-dashboard-diagnose*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== Filter state ===\n")
      (insert (format "sections: %S\n" sections))
      (insert (format "attention-categories: %S\n" att-cats))
      (insert (format "repos: %S\n" repos))
      (insert (format "status: %S\n" status))
      (insert (format "period: %S\n" period))
      (insert (format "excluded-assignees: %S\n" org-github-dashboard-excluded-assignees))
      (insert (format "excluded-milestones: %S\n" org-github-dashboard-excluded-milestones))
      (insert "\n=== Attention counts ===\n")
      (dolist (cat att-cats)
        (let* ((q (pcase cat
                    ('overdue (org-github-dashboard--filtered-issue-query
                               '(todo) '(deadline :to -1)))
                    ('no-deadline (org-github-dashboard--filtered-issue-query
                                   '(todo) '(not (deadline))))
                    ('no-milestone (org-github-dashboard--filtered-issue-query
                                    '(todo) '(not (property "MILESTONE"))))
                    ('stale (org-github-dashboard--filtered-issue-query
                             '(todo) `(github-stale ,org-github-dashboard-stale-days)))))
               (n (condition-case err
                      (org-github-dashboard--count-query q)
                    (error (format "ERROR: %S" err)))))
          (insert (format "  %-14s %s   (query: %S)\n" cat n q))))
      (insert "\n=== Assignees (open count) ===\n")
      (dolist (a assignees)
        (let ((n (org-github-dashboard--count-query
                  (org-github-dashboard--filtered-issue-query
                   '(todo) `(github-assignee ,a)))))
          (insert (format "  %-30s %d open\n" a n))))
      (insert "\n=== Milestones (total count) ===\n")
      (dolist (m milestones)
        (let ((n (org-github-dashboard--count-query
                  (org-github-dashboard--filtered-issue-query
                   `(property "MILESTONE" ,m)))))
          (insert (format "  %-50s %d items\n" m n))))
      (insert (format "\n=== Block list ===\n%d blocks total\n"
                      (length (org-github-dashboard--build-blocks))))
      (goto-char (point-min)))
    (display-buffer buf)))

(defun org-github-dashboard--run (&rest _)
  "Agenda function: build dynamic blocks and run as composite agenda."
  (let* ((blocks (org-github-dashboard--build-blocks)))
    (setq org-agenda-custom-commands
          (cons `("g!" "GitHub Team Dashboard (dynamic)" ,blocks)
                (assoc-delete-all "g!" org-agenda-custom-commands)))
    (org-agenda-run-series "GitHub Team Dashboard" (list blocks))
    (when (memq 'summary org-github-dashboard-sections)
      (org-github-dashboard--insert-summary-header))
    (org-github-dashboard--fixup-done-dates)
    (org-github-dashboard--restyle-section-titles)
    (local-set-key (kbd "/") #'org-github-dashboard-toggle-filter)
    (local-set-key (kbd "k") #'org-github-dashboard-kanban)
    (local-set-key (kbd "S") #'org-github-dashboard-sync-item)
    (local-set-key (kbd "s-z H P") #'org-github-dashboard-sync-item)
    (local-set-key (kbd "C-d") #'org-github-dashboard-set-deadline)
    (local-set-key (kbd "V d") #'org-github-dashboard-view-day)
    (local-set-key (kbd "V w") #'org-github-dashboard-view-week)
    (local-set-key (kbd "V m") #'org-github-dashboard-view-month)
    (local-set-key (kbd "V a") #'org-github-dashboard-view-all)
    (local-set-key (kbd "TAB") #'org-github-dashboard-toggle-fold)
    (local-set-key (kbd "<tab>") #'org-github-dashboard-toggle-fold)
    (local-set-key [tab] #'org-github-dashboard-toggle-fold)
    (local-set-key (kbd "<backtab>") #'org-github-dashboard-cycle-all)
    (local-set-key (kbd "S-TAB") #'org-github-dashboard-cycle-all)
    (local-set-key [backtab] #'org-github-dashboard-cycle-all)
    (local-set-key [S-tab] #'org-github-dashboard-cycle-all)
    (goto-char (point-min))
    (when (get-buffer-window (current-buffer))
      (set-window-start (get-buffer-window (current-buffer)) (point-min)))))

;;; Interactive Commands

;;;###autoload
(defun org-github-dashboard-toggle-filter ()
  "Interactively filter the GitHub dashboard by repos, assignees, milestones, status, or period."
  (interactive)
  (let ((dimension (completing-read "Filter by: "
                                    '("repos" "assignees" "milestones" "status" "period" "clear all")
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
      ("milestones"
       (let* ((all-milestones (seq-remove
                               (lambda (m) (member m org-github-dashboard-excluded-milestones))
                               (org-github-dashboard--collect-milestones)))
              (selected (completing-read-multiple
                         "Include milestones (comma-separated, empty=all): "
                         all-milestones nil nil
                         (when org-github-dashboard-milestones
                           (string-join org-github-dashboard-milestones ",")))))
         (setq org-github-dashboard-milestones
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
             org-github-dashboard-milestones nil
             org-github-dashboard-status 'all
             org-github-dashboard-period nil)))
    (cond ((derived-mode-p 'org-agenda-mode)
           (org-github-dashboard))
          ((derived-mode-p 'org-github-dashboard-kanban-mode)
           (org-github-dashboard-kanban-refresh)))))

;;;###autoload
(defun org-github-dashboard-sync-item ()
  "Asynchronously sync the issue or PR at point in the dashboard.
Fetches from GitHub without blocking Emacs, then refreshes the dashboard
when done.  With \\[universal-argument], force-pull; with \\[universal-argument] \\[universal-argument], force-push."
  (interactive)
  (let* ((marker (or (org-get-at-bol 'org-hd-marker)
                     (org-agenda-error)))
         (agenda-buf (current-buffer)))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char (marker-position marker))
        (org-github-sync-at-point-async
         current-prefix-arg
         (lambda (_error)
           (when (buffer-live-p agenda-buf)
             (with-current-buffer agenda-buf
               (org-agenda-redo t)))))))))

;;;###autoload
(defun org-github-dashboard-set-deadline ()
  "Set or remove the deadline for the item at point.
Uses the standard Org date picker.  If the item belongs to a configured
GitHub Projects V2 board, the deadline is also pushed there asynchronously
after the dashboard refreshes."
  (interactive)
  (let* ((marker (or (org-get-at-bol 'org-hd-marker)
                     (org-agenda-error)))
         (buf (marker-buffer marker))
         (pos (marker-position marker))
         (agenda-buf (current-buffer)))
    ;; Set deadline interactively in the source org buffer
    (with-current-buffer buf
      (save-excursion
        (goto-char pos)
        (call-interactively #'org-deadline)))
    ;; Capture updated deadline and item identifiers after the picker closes
    (let* ((deadline-str (with-current-buffer buf
                           (save-excursion (goto-char pos)
                                           (org-entry-get (point) "DEADLINE"))))
           (repo (with-current-buffer buf
                   (save-excursion (goto-char pos)
                                   (org-entry-get (point) "REPO"))))
           (num-str (with-current-buffer buf
                      (save-excursion (goto-char pos)
                                      (or (org-entry-get (point) "ISSUE_NUMBER")
                                          (org-entry-get (point) "PR_NUMBER"))))))
      ;; Defer the dashboard redo slightly so Emacs can redisplay first
      (run-with-idle-timer
       0.05 nil
       (lambda ()
         (when (buffer-live-p agenda-buf)
           (with-current-buffer agenda-buf
             (org-agenda-redo t)))))
      ;; Push deadline to GitHub Projects V2 if configured
      (when (and deadline-str repo num-str
                 (assoc repo org-github-repo-project-alist))
        (let ((num (string-to-number num-str)))
          (when (> num 0)
            (message "Pushing deadline to GitHub Projects V2...")
            (org-github--push-deadline-async
             repo num deadline-str
             (lambda (err)
               (if err
                   (message "org-github: deadline push failed: %s" err)
                 (message "Deadline pushed to GitHub Projects V2"))))))))))

;;;###autoload
(defun org-github-dashboard ()
  "Show GitHub team dashboard with dynamically discovered assignees."
  (interactive)
  (org-agenda nil "gt")
  (delete-other-windows))

;;; Discord Integration

(defun org-github-dashboard--issues-with-open-prs (repos)
  "Return a hash-set of (REPO . ISSUE-NUMBER) pairs that have an open PR.
Scans open PR body text for closing keywords (Fixes, Closes, Resolves)
referencing issue numbers."
  (let* ((org-github-dashboard-repos repos)
         (query (org-github-dashboard--filtered-issue-query '(todo) '(github-pr)))
         (linked (make-hash-table :test 'equal))
         (pr-data (org-ql-select (org-agenda-files) query
                    :action (lambda ()
                              (let ((repo (org-entry-get (point) "REPO"))
                                    (body (org-get-entry)))
                                (when (and repo body)
                                  (cons repo body)))))))
    (dolist (entry (delq nil pr-data))
      (let ((repo (car entry))
            (body (cdr entry)))
        (with-temp-buffer
          (insert body)
          (goto-char (point-min))
          (while (re-search-forward
                  "\\b\\(?:[Ff]ix\\(?:e[sd]\\)?\\|[Cc]lose[sd]?\\|[Rr]esolve[sd]?\\)\\s-+#\\([0-9]+\\)"
                  nil t)
            (puthash (cons repo (string-to-number (match-string 1))) t linked)))))
    linked))

(defun org-github-dashboard--collect-discord-items (repos target-date)
  "Collect open GitHub items from REPOS due on or before TARGET-DATE.
TARGET-DATE is a \"YYYY-MM-DD\" string.  Returns an alist of
((ASSIGNEE . ((:title T :type issue|pr :number N
               :repo R :deadline D :overdue BOOL) ...)) ...).
Items without a DEADLINE are included under a \"No Deadline\" section
when they are open."
  (let* ((org-github-dashboard-repos repos)
         (today (format-time-string "%Y-%m-%d"))
         (pr-linked (when org-github-dashboard-discord-hide-pr-linked
                      (org-github-dashboard--issues-with-open-prs repos)))
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
                              ;; Skip issues linked to an open PR
                              (unless (and iss-num pr-linked
                                           (gethash (cons repo (string-to-number iss-num)) pr-linked))
                                ;; Include if: has deadline <= target-date, OR has no deadline (unless hidden)
                                (when (or (and dl-date (not (string< target-date dl-date)))
                                          (and (null dl-date)
                                               (not org-github-dashboard-discord-hide-no-deadline)))
                                  (list :title title :repo repo
                                        :type (if pr-num 'pr 'issue)
                                        :number (string-to-number (or pr-num iss-num "0"))
                                        :assignees (when assignees-raw
                                                     (split-string assignees-raw "," t " +"))
                                        :deadline dl-date
                                        :overdue (and dl-date (string< dl-date today)))))))))
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
            (push (format "> :red_circle: **%d overdue**" (length overdue)) lines)
            (dolist (item overdue)
              (push (format "- :red_circle: **%s** — %s #%d (was due %s)"
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

(defun org-github-dashboard--webhook-for-repo (repo)
  "Return the Discord webhook URL for REPO.
Looks up REPO in `org-github-dashboard-discord-webhook-alist',
falling back to `org-github-dashboard-discord-webhook-url'."
  (or (cdr (assoc repo org-github-dashboard-discord-webhook-alist))
      org-github-dashboard-discord-webhook-url))

(defun org-github-dashboard--send-chunks-async (chunks url done-callback)
  "Send CHUNKS sequentially to Discord webhook URL, async.
DONE-CALLBACK receives t on full success or nil on any failure."
  (if (null chunks)
      (funcall done-callback t)
    (let* ((url-request-method "POST")
           (url-request-extra-headers '(("Content-Type" . "application/json")))
           (payload (json-encode `((content . ,(car chunks)))))
           (url-request-data (encode-coding-string payload 'utf-8)))
      (url-retrieve
       url
       (lambda (status &rest _)
         (unwind-protect
             (if (plist-get status :error)
                 (progn
                   (message "Discord webhook error: %s" (plist-get status :error))
                   (funcall done-callback nil))
               (goto-char (point-min))
               (if (and (re-search-forward "HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
                        (member (match-string 1) '("200" "204")))
                   (progn
                     (message "Discord: sent %d chars" (length (car chunks)))
                     (org-github-dashboard--send-chunks-async
                      (cdr chunks) url done-callback))
                 (message "Discord webhook failed: %s"
                          (buffer-substring (point-min) (min (point-max) 500)))
                 (funcall done-callback nil)))
           (kill-buffer (current-buffer))))
       nil t))))

(defun org-github-dashboard--send-discord-webhook (message &optional url)
  "Send MESSAGE to Discord webhook at URL.
URL defaults to `org-github-dashboard-discord-webhook-url'.
Splits into multiple messages if MESSAGE exceeds Discord's 2000
character limit.  Sends asynchronously and returns immediately."
  (let ((url (or url
                 org-github-dashboard-discord-webhook-url
                 (user-error "Set `org-github-dashboard-discord-webhook-url' first")))
        (chunks (org-github-dashboard--split-message message 2000)))
    (org-github-dashboard--send-chunks-async
     chunks url
     (lambda (ok)
       (when ok
         (message "Discord message sent successfully (%d part%s)."
                  (length chunks) (if (= (length chunks) 1) "" "s")))))))

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
         (per-repo-p (and org-github-dashboard-discord-webhook-alist
                          (> (length repo-list) 0))))
    (if per-repo-p
        ;; Per-repo mode: separate message per repo, each to its own webhook
        (let ((preview-parts nil)
              (send-queue nil))
          (dolist (repo repo-list)
            (let ((msg (org-github-dashboard--format-discord-message
                        (list repo) target-date since-date))
                  (url (org-github-dashboard--webhook-for-repo repo)))
              (push (format "--- %s (-> %s) ---\n%s"
                            repo
                            (if url (substring url 0 (min 40 (length url))) "NONE")
                            msg)
                    preview-parts)
              (push (cons msg url) send-queue)))
          (setq preview-parts (nreverse preview-parts))
          (setq send-queue (nreverse send-queue))
          (with-current-buffer (get-buffer-create "*Discord Preview*")
            (erase-buffer)
            (insert (string-join preview-parts "\n\n"))
            (goto-char (point-min))
            (display-buffer (current-buffer)))
          (when (y-or-n-p (format "Send %d per-repo messages to Discord? "
                                  (length send-queue)))
            (dolist (entry send-queue)
              (org-github-dashboard--send-discord-webhook (car entry) (cdr entry))))
          (when-let ((win (get-buffer-window "*Discord Preview*")))
            (delete-window win))
          (kill-buffer "*Discord Preview*"))
      ;; Legacy mode: single message to single webhook
      (let ((msg (org-github-dashboard--format-discord-message
                  repo-list target-date since-date)))
        (with-current-buffer (get-buffer-create "*Discord Preview*")
          (erase-buffer)
          (insert msg)
          (goto-char (point-min))
          (display-buffer (current-buffer)))
        (when (y-or-n-p "Send this message to Discord? ")
          (org-github-dashboard--send-discord-webhook msg))
        (when-let ((win (get-buffer-window "*Discord Preview*")))
          (delete-window win))
        (kill-buffer "*Discord Preview*")))))

;;; Email Integration

(defun org-github-dashboard--html-escape (s)
  "Escape HTML-special characters in string S."
  (if (null s)
      ""
    (replace-regexp-in-string
     "[&<>]"
     (lambda (m) (cond ((string= m "&") "&amp;")
                       ((string= m "<") "&lt;")
                       (t "&gt;")))
     s t t)))

(defun org-github-dashboard--email-motivation (total-due total-overdue total-completed)
  "Return a context-aware motivational line.
Celebrates when nothing is due or overdue, acknowledges completed
work, rallies when the overdue or due load is heavy, and otherwise
falls back to a random line from
`org-github-dashboard-email-motivations'."
  (cond
   ((and (zerop total-due) (> total-completed 0))
    (format "Nothing due and %d already wrapped up — beautiful work. 🎉"
            total-completed))
   ((zerop total-due)
    "A clear board today. Enjoy the calm — you earned it. 🌤️")
   ((and (> total-overdue 0) (>= total-overdue (/ total-due 2)))
    (format "%d overdue, but every one you close is momentum back in your favor. Let's go. 💪"
            total-overdue))
   ((> total-overdue 0)
    "A few are running late — knock those out first and the rest will feel easy. 🚀")
   ((> total-due 8)
    "A full plate today. Pick the top three and let the rest follow. 🎯")
   (t
    (let ((lines org-github-dashboard-email-motivations))
      (if lines
          (nth (random (length lines)) lines)
        "Keep going — you're doing great.")))))

(defun org-github-dashboard--email-item-html (item &optional overdue done)
  "Return an HTML <li> string for ITEM.
When OVERDUE is non-nil, style it as overdue.  When DONE is non-nil,
style it as completed (strikethrough)."
  (let* ((title (org-github-dashboard--html-escape (plist-get item :title)))
         (type (if (eq (plist-get item :type) 'pr) "PR" "Issue"))
         (num (plist-get item :number))
         (meta (cond
                (done (format " <span style=\"color:#888;\">(closed %s)</span>"
                              (plist-get item :closed-date)))
                (overdue (format " <span style=\"color:#c0392b;\">(was due %s)</span>"
                                 (plist-get item :deadline)))
                ((plist-get item :deadline)
                 (format " <span style=\"color:#888;\">(due %s)</span>"
                         (plist-get item :deadline)))
                (t "")))
         (label (format "%s #%d" type num)))
    (cond
     (done
      (format "<li style=\"margin:2px 0;color:#888;\"><s>%s</s> <span style=\"color:#aaa;\">%s</span>%s</li>"
              title label meta))
     (overdue
      (format "<li style=\"margin:2px 0;\"><span style=\"color:#c0392b;\">🔴</span> <strong>%s</strong> <span style=\"color:#888;\">%s</span>%s</li>"
              title label meta))
     (t
      (format "<li style=\"margin:2px 0;\">%s <span style=\"color:#888;\">%s</span>%s</li>"
              title label meta)))))

(defun org-github-dashboard--format-email-html (repos &optional target-date since-date)
  "Build an HTML summary for REPOS, mirroring the Discord summary.
With TARGET-DATE (a \"YYYY-MM-DD\" string), include open items due on
or before that date grouped by assignee with overdue items flagged.
With SINCE-DATE, append a completed-since section.  With both nil,
produce the all-open-items weekly summary.  A context-aware
motivational banner is prepended.  Returns a cons (SUBJECT . HTML)."
  (let* ((body "") (subject "") (banner-stats (list 0 0 0)))
    (if (null target-date)
        ;; All-open weekly summary
        (let* ((org-github-dashboard-repos repos)
               (all-assignees (seq-remove
                               (lambda (a) (member a org-github-dashboard-excluded-assignees))
                               (org-github-dashboard--collect-assignees)))
               (assignees (if org-github-dashboard-assignees
                              (seq-filter (lambda (a) (member a org-github-dashboard-assignees))
                                          all-assignees)
                            all-assignees))
               (mon (format-time-string "%b %d"))
               (sun (format-time-string "%b %d" (time-add (current-time) (days-to-time 6))))
               (rows "") (total-oi 0) (total-op 0))
          (setq subject (format "Weekly GitHub Summary — %s to %s" mon sun))
          (dolist (name assignees)
            (let* ((s (org-github-dashboard--collect-all-stats name))
                   (oi (plist-get s :open-issues))
                   (op (plist-get s :open-prs)))
              (cl-incf total-oi oi)
              (cl-incf total-op op)
              (when (> (+ oi op) 0)
                (setq rows
                      (concat rows
                              (format "<li style=\"margin:3px 0;\"><strong>%s</strong> — %d open issue%s, %d open PR%s</li>"
                                      (org-github-dashboard--html-escape name)
                                      oi (if (= oi 1) "" "s") op (if (= op 1) "" "s")))))))
          (setq banner-stats (list (+ total-oi total-op) 0 0))
          (setq body
                (concat
                 "<h2 style=\"margin:0 0 12px;color:#24292e;\">Weekly GitHub Summary</h2>"
                 (format "<p style=\"color:#666;margin:0 0 12px;\">%s to %s</p>" mon sun)
                 (if (string-empty-p rows)
                     "<p>No open items. 🎉</p>"
                   (format "<ul style=\"list-style:none;padding:0;margin:0 0 12px;\">%s</ul>" rows))
                 (format "<p style=\"border-top:1px solid #eee;padding-top:10px;margin-top:10px;\"><strong>Team Total</strong> — %d open issue%s, %d open PR%s</p>"
                         total-oi (if (= total-oi 1) "" "s")
                         total-op (if (= total-op 1) "" "s")))))
      ;; Date-filtered summary
      (let* ((by-assignee (org-github-dashboard--collect-discord-items repos target-date))
             (date-label (format-time-string
                          "%a, %b %d"
                          (date-to-time (concat target-date "T00:00:00Z"))))
             (sections "") (total-due 0) (total-overdue 0))
        (setq subject (format "GitHub Tasks Due by %s" date-label))
        (dolist (entry by-assignee)
          (let* ((name (car entry))
                 (items (cdr entry))
                 (overdue (seq-filter (lambda (i) (plist-get i :overdue)) items))
                 (due-on (seq-filter (lambda (i) (and (plist-get i :deadline)
                                                      (not (plist-get i :overdue)))) items))
                 (no-deadline (seq-filter (lambda (i) (null (plist-get i :deadline))) items))
                 (item-html ""))
            (cl-incf total-due (length items))
            (cl-incf total-overdue (length overdue))
            (dolist (it overdue)
              (setq item-html (concat item-html
                                      (org-github-dashboard--email-item-html it t nil))))
            (dolist (it due-on)
              (setq item-html (concat item-html
                                      (org-github-dashboard--email-item-html it nil nil))))
            (dolist (it no-deadline)
              (setq item-html (concat item-html
                                      (org-github-dashboard--email-item-html it nil nil))))
            (setq sections
                  (concat sections
                          (format "<h3 style=\"margin:16px 0 4px;color:#24292e;\">%s <span style=\"font-weight:normal;color:#888;\">(%d item%s%s)</span></h3>"
                                  (org-github-dashboard--html-escape name)
                                  (length items) (if (= (length items) 1) "" "s")
                                  (if overdue (format ", %d overdue" (length overdue)) ""))
                          (format "<ul style=\"list-style:none;padding:0;margin:0;\">%s</ul>" item-html)))))
        (when (zerop total-due)
          (setq sections "<p>No tasks due by this date. 🎉</p>"))
        (setq banner-stats (list total-due total-overdue 0))
        (setq body
              (concat
               (format "<h2 style=\"margin:0 0 12px;color:#24292e;\">GitHub Tasks Due by %s</h2>" date-label)
               sections
               (format "<p style=\"border-top:1px solid #eee;padding-top:10px;margin-top:14px;\"><strong>Total: %d task%s due, %d overdue</strong></p>"
                       total-due (if (= total-due 1) "" "s") total-overdue)))
        ;; Completed section
        (when since-date
          (let* ((by-assignee-done (org-github-dashboard--collect-discord-completed repos since-date))
                 (since-label (format-time-string
                               "%a, %b %d"
                               (date-to-time (concat since-date "T00:00:00Z"))))
                 (done-sections "") (total-completed 0))
            (dolist (entry by-assignee-done)
              (let* ((name (car entry))
                     (items (cdr entry))
                     (item-html ""))
                (cl-incf total-completed (length items))
                (dolist (it items)
                  (setq item-html (concat item-html
                                          (org-github-dashboard--email-item-html it nil t))))
                (setq done-sections
                      (concat done-sections
                              (format "<h3 style=\"margin:16px 0 4px;color:#24292e;\">%s <span style=\"font-weight:normal;color:#888;\">(%d completed)</span></h3>"
                                      (org-github-dashboard--html-escape name) (length items))
                              (format "<ul style=\"list-style:none;padding:0;margin:0;\">%s</ul>" item-html)))))
            (setcar (nthcdr 2 banner-stats) total-completed)
            (setq body
                  (concat
                   body
                   (format "<hr style=\"border:none;border-top:1px solid #ddd;margin:20px 0;\">")
                   (format "<h2 style=\"margin:0 0 12px;color:#24292e;\">Completed Since %s</h2>" since-label)
                   (if (zerop total-completed)
                       "<p>No items completed in this period.</p>"
                     (concat done-sections
                             (format "<p style=\"margin-top:10px;\"><strong>Total: %d completed</strong></p>"
                                     total-completed)))))))))
    ;; Assemble the full document with banner + footer
    (let* ((motivation (apply #'org-github-dashboard--email-motivation banner-stats))
           (footer (format "<p style=\"color:#999;font-size:12px;margin-top:24px;border-top:1px solid #eee;padding-top:8px;\">Repos: %s</p>"
                           (org-github-dashboard--html-escape
                            (string-join (or repos '("all")) ", "))))
           (html (concat
                  "<div style=\"font-family:-apple-system,Segoe UI,Helvetica,Arial,sans-serif;max-width:640px;margin:0 auto;color:#24292e;line-height:1.5;\">"
                  (format "<div style=\"background:linear-gradient(135deg,#6e8efb,#a777e3);color:#fff;padding:16px 20px;border-radius:8px;margin-bottom:20px;\"><div style=\"font-size:15px;font-weight:600;\">✨ %s</div></div>"
                          (org-github-dashboard--html-escape motivation))
                  body
                  footer
                  "</div>")))
      (cons subject html))))

(defun org-github-dashboard--copy-html-to-clipboard (html)
  "Put HTML on the system clipboard as a text/html flavor.
Prefers wl-copy (Wayland), falling back to xclip (X11).  Returns the
name of the tool used, or nil when no clipboard tool is available."
  (let ((spec (cond
               ((executable-find "wl-copy")
                '("wl-copy" "wl-copy" "--type" "text/html"))
               ((executable-find "xclip")
                '("xclip" "xclip" "-selection" "clipboard" "-t" "text/html"))
               (t nil))))
    (when spec
      (let ((proc (make-process :name (car spec)
                                :command (cdr spec)
                                :connection-type 'pipe
                                :noquery t)))
        (process-send-string proc html)
        (process-send-eof proc))
      (car spec))))

;;;###autoload
(defun org-github-dashboard-compose-email (target-date since-date)
  "Render a GitHub summary email for previewing and pasting into Gmail.
TARGET-DATE is a \"YYYY-MM-DD\" string for the due-by cutoff.
SINCE-DATE is a \"YYYY-MM-DD\" string; completed items closed on or
after this date are included.  Interactively, both use the Org date
picker.  With prefix arg (C-u), builds the all-open-items weekly
summary instead.  Prompts for repos if `org-github-dashboard-repos'
is not set.

The body is HTML (a context-aware motivational banner plus the
summary).  Opens a rendered preview in the browser and copies the
email's rich HTML to the clipboard as a text/html flavor, so pasting
\(C-v) into a Gmail compose window preserves the styling.  Add To/Cc
and send from the browser."
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
         (result (org-github-dashboard--format-email-html
                  repo-list target-date since-date))
         (subject (car result))
         (html (cdr result))
         (esc-subject (org-github-dashboard--html-escape subject))
         (tool (org-github-dashboard--copy-html-to-clipboard html))
         (page (concat
                "<!DOCTYPE html><html><head><meta charset=\"utf-8\">"
                (format "<title>%s</title>" esc-subject)
                "</head><body style=\"margin:0;padding:24px;background:#f6f8fa;\">"
                "<div style=\"max-width:680px;margin:0 auto 16px;font-family:-apple-system,Segoe UI,Helvetica,Arial,sans-serif;\">"
                (format "<div style=\"font-size:12px;color:#888;\">Subject</div><div style=\"font-size:16px;font-weight:600;color:#24292e;\">%s</div>"
                        esc-subject)
                (format "<div style=\"margin-top:8px;font-size:12px;color:%s;\">%s</div>"
                        (if tool "#2da44e" "#c0392b")
                        (if tool
                            (format "Rich HTML copied to clipboard via %s — paste (C-v) into Gmail, then add To/Cc and send."
                                    tool)
                          "No clipboard tool found — select all (C-a) and copy (C-c) from the email below to paste into Gmail."))
                "</div>"
                "<div style=\"max-width:680px;margin:0 auto;background:#fff;padding:24px;border-radius:8px;border:1px solid #e1e4e8;\">"
                html
                "</div></body></html>"))
         (file (make-temp-file "org-github-email-" nil ".html" page)))
    (browse-url-of-file file)
    (if tool
        (message "Email ready: \"%s\" — preview opened, HTML on clipboard (%s). Paste into Gmail."
                 subject tool)
      (message "Email ready: \"%s\" — preview opened. Copy from the browser to paste into Gmail."
               subject))))

;;; Mermaid Gantt Chart

(defun org-github-dashboard--gantt-task-id (repo type number)
  "Create a Mermaid-safe task ID from REPO, TYPE, and NUMBER.
Replaces slashes, dashes, and other special characters with underscores."
  (replace-regexp-in-string
   "[^a-zA-Z0-9_]" "_"
   (format "%s_%s_%s" repo type number)))

(defun org-github-dashboard--collect-gantt-items ()
  "Collect task plists for Gantt chart generation.
Return list of (:title :repo :type :number :state :created :deadline :assignee).
State is one of `done', `active', or `crit' (open + past deadline).
Respects all dashboard filters."
  (let ((today (format-time-string "%Y-%m-%d")))
    (org-ql-select (org-agenda-files)
      (org-github-dashboard--filtered-issue-query)
      :action
      (lambda ()
        (let* ((is-pr (org-entry-get (point) "PR_NUMBER"))
               (number (or is-pr (org-entry-get (point) "ISSUE_NUMBER")))
               (type (if is-pr "pr" "issue"))
               (repo (org-entry-get (point) "REPO"))
               (title (org-get-heading t t t t))
               (is-done (org-entry-is-done-p))
               (created-raw (org-entry-get (point) "CREATED_AT"))
               (deadline-raw (org-entry-get (point) "DEADLINE"))
               (created (or (org-github-dashboard--date-from-ts created-raw)
                            (format-time-string
                             "%Y-%m-%d"
                             (time-subtract (current-time)
                                            (days-to-time org-github-dashboard-gantt-default-duration)))))
               (deadline (or (when deadline-raw
                               (if (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" deadline-raw)
                                   (match-string 1 deadline-raw)))
                             today))
               (assignees (org-entry-get (point) "ASSIGNEES"))
               (state (cond
                       (is-done "done")
                       ((string< deadline today) "crit")
                       (t "active"))))
          (list :title title :repo repo :type type :number number
                :state state :created created :deadline deadline
                :assignee (or assignees "")))))))

(defun org-github-dashboard--format-gantt (items)
  "Build a Mermaid Gantt chart string from ITEMS.
ITEMS is a list of plists as returned by `--collect-gantt-items'.
Groups tasks by repository as sections."
  (let ((grouped (make-hash-table :test 'equal))
        (repos '())
        (lines '()))
    ;; Group items by repo
    (dolist (item items)
      (let ((repo (plist-get item :repo)))
        (unless (gethash repo grouped)
          (push repo repos))
        (push item (gethash repo grouped))))
    (setq repos (nreverse repos))
    ;; Build Mermaid output
    (push "gantt" lines)
    (push "    dateFormat YYYY-MM-DD" lines)
    (push "    axisFormat %b %d" lines)
    (dolist (repo repos)
      (push (format "    section %s" repo) lines)
      (dolist (item (nreverse (gethash repo grouped)))
        (let* ((title (plist-get item :title))
               (truncated (if (> (length title) 30)
                              (concat (substring title 0 27) "...")
                            title))
               (safe-title (replace-regexp-in-string ":" "-" truncated))
               (id (org-github-dashboard--gantt-task-id
                    repo (plist-get item :type) (plist-get item :number)))
               (state (plist-get item :state))
               (created (plist-get item :created))
               (deadline (plist-get item :deadline)))
          (push (format "    %s :%s, %s, %s, %s"
                        safe-title state id created deadline)
                lines))))
    (mapconcat #'identity (nreverse lines) "\n")))

;;;###autoload
(defun org-github-dashboard-gantt ()
  "Insert a Mermaid Gantt chart of GitHub tasks at point.
Respects all dashboard filters (repos, assignees, status).
The chart is inserted as an org-mode source block."
  (interactive)
  (let* ((items (org-github-dashboard--collect-gantt-items))
         (chart (org-github-dashboard--format-gantt items)))
    (insert "#+begin_src mermaid\n" chart "\n#+end_src\n")))

;;; Investor Report

(defun org-github-dashboard--collect-repo-stats ()
  "Collect per-repository statistics.
Return alist of (REPO . (:open-issues N :open-prs N :done-issues N :done-prs N))."
  (let ((repos (or org-github-dashboard-repos
                   (org-github-dashboard--collect-repos))))
    (mapcar
     (lambda (repo)
       (let ((org-github-dashboard-repos (list repo)))
         (cons repo (org-github-dashboard--collect-all-stats nil))))
     repos)))

(defun org-github-dashboard--collect-assignee-stats ()
  "Collect per-assignee statistics.
Return alist of (ASSIGNEE . (:open-issues N :open-prs N :done-issues N :done-prs N))."
  (let ((assignees (seq-remove
                    (lambda (a) (member a org-github-dashboard-excluded-assignees))
                    (if org-github-dashboard-assignees
                        org-github-dashboard-assignees
                      (org-github-dashboard--collect-assignees)))))
    (mapcar
     (lambda (name)
       (cons name (org-github-dashboard--collect-all-stats name)))
     assignees)))

(defun org-github-dashboard--collect-overdue-items ()
  "Collect open items with deadline before today.
Return list of plists (:title :repo :type :number :deadline :assignees)."
  (let ((today (format-time-string "%Y-%m-%d")))
    (org-ql-select (org-agenda-files)
      (org-github-dashboard--filtered-issue-query '(todo) '(deadline :to -1))
      :action
      (lambda ()
        (let* ((is-pr (org-entry-get (point) "PR_NUMBER"))
               (number (or is-pr (org-entry-get (point) "ISSUE_NUMBER")))
               (type (if is-pr "PR" "Issue"))
               (deadline-raw (org-entry-get (point) "DEADLINE"))
               (deadline (when deadline-raw
                           (if (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" deadline-raw)
                               (match-string 1 deadline-raw)
                             ""))))
          (list :title (org-get-heading t t t t)
                :repo (org-entry-get (point) "REPO")
                :type type
                :number number
                :deadline (or deadline "N/A")
                :assignees (or (org-entry-get (point) "ASSIGNEES") "Unassigned")))))))

(defun org-github-dashboard--collect-upcoming-items (&optional days)
  "Collect open items with deadline within DAYS (default 14) from today.
Return list of plists (:title :repo :type :number :deadline :assignees)."
  (let ((days (or days 14)))
    (org-ql-select (org-agenda-files)
      (org-github-dashboard--filtered-issue-query
       '(todo)
       `(deadline :from 0 :to ,days))
      :action
      (lambda ()
        (let* ((is-pr (org-entry-get (point) "PR_NUMBER"))
               (number (or is-pr (org-entry-get (point) "ISSUE_NUMBER")))
               (type (if is-pr "PR" "Issue"))
               (deadline-raw (org-entry-get (point) "DEADLINE"))
               (deadline (when deadline-raw
                           (if (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" deadline-raw)
                               (match-string 1 deadline-raw)
                             ""))))
          (list :title (org-get-heading t t t t)
                :repo (org-entry-get (point) "REPO")
                :type type
                :number number
                :deadline (or deadline "N/A")
                :assignees (or (org-entry-get (point) "ASSIGNEES") "Unassigned")))))))

(defun org-github-dashboard--format-investor-report
    (repo-stats assignee-stats overdue-items upcoming-items gantt-items)
  "Build an org-mode investor report string.
REPO-STATS is from `--collect-repo-stats'.
ASSIGNEE-STATS is from `--collect-assignee-stats'.
OVERDUE-ITEMS and UPCOMING-ITEMS are plists from their collectors.
GANTT-ITEMS is from `--collect-gantt-items'."
  (let ((total-open 0) (total-done 0) (lines '()))
    ;; Calculate totals
    (dolist (entry repo-stats)
      (let ((s (cdr entry)))
        (cl-incf total-open (+ (plist-get s :open-issues) (plist-get s :open-prs)))
        (cl-incf total-done (+ (plist-get s :done-issues) (plist-get s :done-prs)))))
    (let ((total (+ total-open total-done))
          (rate (if (> (+ total-open total-done) 0)
                    (/ (* 100.0 total-done) (+ total-open total-done))
                  0.0)))
      ;; Title
      (push (format "#+TITLE: %s" org-github-dashboard-report-title) lines)
      (push (format "#+DATE: %s" (format-time-string "%Y-%m-%d")) lines)
      (push "" lines)

      ;; Executive Summary
      (push "* Executive Summary" lines)
      (push "" lines)
      (push "| Metric | Count |" lines)
      (push "|--------+-------|" lines)
      (push (format "| Total Items | %d |" total) lines)
      (push (format "| Open | %d |" total-open) lines)
      (push (format "| Completed | %d |" total-done) lines)
      (push (format "| Completion Rate | %.1f%% |" rate) lines)
      (push (format "| Overdue | %d |" (length overdue-items)) lines)
      (push (format "| Upcoming (14d) | %d |" (length upcoming-items)) lines)
      (push "" lines)

      ;; Repository Breakdown
      (push "* Repository Breakdown" lines)
      (push "" lines)
      (push "| Repository | Open Issues | Open PRs | Done Issues | Done PRs | Completion |" lines)
      (push "|------------+-------------+----------+-------------+----------+------------|" lines)
      (dolist (entry repo-stats)
        (let* ((repo (car entry))
               (s (cdr entry))
               (oi (plist-get s :open-issues))
               (op (plist-get s :open-prs))
               (di (plist-get s :done-issues))
               (dp (plist-get s :done-prs))
               (repo-total (+ oi op di dp))
               (repo-rate (if (> repo-total 0) (/ (* 100.0 (+ di dp)) repo-total) 0.0)))
          (push (format "| %s | %d | %d | %d | %d | %.1f%% |" repo oi op di dp repo-rate) lines)))
      (push "" lines)

      ;; Team Workload
      (push "* Team Workload" lines)
      (push "" lines)
      (dolist (entry assignee-stats)
        (let* ((name (car entry))
               (s (cdr entry))
               (open (+ (plist-get s :open-issues) (plist-get s :open-prs)))
               (done (+ (plist-get s :done-issues) (plist-get s :done-prs))))
          (push (format "- *%s*: %d open, %d done" name open done) lines)))
      (push "" lines)

      ;; Overdue Items
      (push "* Overdue Items" lines)
      (push "" lines)
      (if overdue-items
          (progn
            (push "| # | Type | Title | Repo | Deadline | Assignees |" lines)
            (push "|---+------+-------+------+----------+-----------|" lines)
            (dolist (item overdue-items)
              (push (format "| %s | %s | %s | %s | %s | %s |"
                            (plist-get item :number)
                            (plist-get item :type)
                            (plist-get item :title)
                            (plist-get item :repo)
                            (plist-get item :deadline)
                            (plist-get item :assignees))
                    lines)))
        (push "No overdue items." lines))
      (push "" lines)

      ;; Upcoming Deadlines
      (push "* Upcoming Deadlines" lines)
      (push "" lines)
      (if upcoming-items
          (progn
            (push "| # | Type | Title | Repo | Deadline | Assignees |" lines)
            (push "|---+------+-------+------+----------+-----------|" lines)
            (dolist (item upcoming-items)
              (push (format "| %s | %s | %s | %s | %s | %s |"
                            (plist-get item :number)
                            (plist-get item :type)
                            (plist-get item :title)
                            (plist-get item :repo)
                            (plist-get item :deadline)
                            (plist-get item :assignees))
                    lines)))
        (push "No upcoming deadlines." lines))
      (push "" lines)

      ;; Project Timeline (Gantt)
      (when org-github-dashboard-report-include-gantt
        (push "* Project Timeline" lines)
        (push "" lines)
        (push "#+begin_src mermaid" lines)
        (push (org-github-dashboard--format-gantt gantt-items) lines)
        (push "#+end_src" lines)
        (push "" lines)))

    (mapconcat #'identity (nreverse lines) "\n")))

;;;###autoload
(defun org-github-dashboard-investor-report ()
  "Generate a GitHub investor status report in a new org-mode buffer.
Collects repository stats, assignee workload, overdue/upcoming items,
and optionally embeds a Mermaid Gantt chart."
  (interactive)
  (let* ((repo-stats (org-github-dashboard--collect-repo-stats))
         (assignee-stats (org-github-dashboard--collect-assignee-stats))
         (overdue (org-github-dashboard--collect-overdue-items))
         (upcoming (org-github-dashboard--collect-upcoming-items))
         (gantt-items (if org-github-dashboard-report-include-gantt
                         (org-github-dashboard--collect-gantt-items)
                       '()))
         (report (org-github-dashboard--format-investor-report
                  repo-stats assignee-stats overdue upcoming gantt-items)))
    (with-current-buffer (get-buffer-create "*GitHub Investor Report*")
      (erase-buffer)
      (insert report)
      (org-mode)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;; Kanban Board View

(defcustom org-github-dashboard-kanban-columns
  '(("To Do"       ("TODO")           "gray70")
    ("In Progress" ("NEXT")           "deep sky blue")
    ("Waiting"     ("WAITING" "HOLD") "orange")
    ("Done"        ("DONE")           "green3"))
  "Columns for the kanban board view.
Each entry is (LABEL (TODO-KEYWORD...) COLOR).  An item is placed in
the first column whose keyword list contains its Org TODO keyword;
items whose keyword matches no column are not shown.  COLOR is used
for the column header and the card border.  Moving a card to a column
sets the item's Org TODO keyword to that column's first keyword."
  :type '(repeat (list (string :tag "Column label")
                       (repeat (string :tag "TODO keyword"))
                       (string :tag "Color")))
  :group 'org-github-dashboard)

(defcustom org-github-dashboard-kanban-min-card-width 18
  "Minimum width in columns of a kanban card."
  :type 'integer :group 'org-github-dashboard)

(defcustom org-github-dashboard-kanban-max-card-width 30
  "Maximum width in columns of a kanban card."
  :type 'integer :group 'org-github-dashboard)

(defface org-github-dashboard-kanban-number
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the issue/PR number on a kanban card."
  :group 'org-github-dashboard)

(defface org-github-dashboard-kanban-title
  '((t :inherit default))
  "Face for the title text on a kanban card."
  :group 'org-github-dashboard)

(defface org-github-dashboard-kanban-meta
  '((t :inherit shadow))
  "Face for the metadata line on a kanban card."
  :group 'org-github-dashboard)

(defface org-github-dashboard-kanban-highlight
  '((t :inherit highlight :extend nil))
  "Face highlighting the currently selected kanban card."
  :group 'org-github-dashboard)

(defvar-local org-github-dashboard--kanban-grid nil
  "List of card geometry plists (:col :row :card :cells :pos) for navigation.")

(defvar-local org-github-dashboard--kanban-cur nil
  "The currently selected kanban grid entry.")

(defvar-local org-github-dashboard--kanban-overlays nil
  "Overlays highlighting the current kanban card.")

(defvar-local org-github-dashboard--kanban-ncols 0
  "Number of columns in the rendered kanban board.")

;;;; Kanban: string helpers

(defun org-github-dashboard--kanban-fit (s width)
  "Return S truncated or space-padded to exactly WIDTH display columns."
  (let ((w (string-width s)))
    (cond ((= w width) s)
          ((< w width) (concat s (make-string (- width w) ?\s)))
          (t (truncate-string-to-width s width nil nil "…")))))

(defun org-github-dashboard--kanban-wrap (text width n)
  "Word-wrap TEXT into N lines of WIDTH columns, ellipsising overflow.
Preserves text properties.  Returns N strings, each padded to WIDTH."
  (let ((words (split-string text " " t)) (lines '()) (cur ""))
    (dolist (w words)
      (let ((cand (if (string-empty-p cur) w (concat cur " " w))))
        (if (<= (string-width cand) width)
            (setq cur cand)
          (unless (string-empty-p cur) (push cur lines))
          (if (> (string-width w) width)
              (progn (push (truncate-string-to-width w width nil nil "…") lines)
                     (setq cur ""))
            (setq cur w)))))
    (unless (string-empty-p cur) (push cur lines))
    (setq lines (nreverse lines))
    (let (out)
      (dotimes (i n)
        (let ((ln (or (nth i lines) "")))
          (when (and (= i (1- n)) (> (length lines) n))
            (setq ln (concat (truncate-string-to-width ln (max 0 (1- width))) "…")))
          (push (org-github-dashboard--kanban-fit ln width) out)))
      (nreverse out))))

(defun org-github-dashboard--kanban-short (name)
  "Shorten an assignee NAME, dropping any org suffix after the first hyphen."
  (if (and name (string-match "\\`\\([^-]+\\)" name)) (match-string 1 name) name))

(defun org-github-dashboard--kanban-meta-line (card inner)
  "Build the metadata line for CARD fitted to INNER width."
  (let* ((assignee (plist-get card :assignee))
         (deadline (plist-get card :deadline))
         (estimate (plist-get card :estimate))
         (done (plist-get card :done))
         (overdue (plist-get card :overdue))
         (left (if assignee
                   (propertize (concat "@" (org-github-dashboard--kanban-short assignee))
                               'face 'org-github-dashboard-kanban-meta)
                 ""))
         (right (if done
                    (propertize "✓" 'face '(:foreground "green3" :weight bold))
                  (concat
                   (if deadline
                       (propertize (concat "⏰" (substring deadline 5))
                                   'face (if overdue '(:foreground "red2")
                                           'org-github-dashboard-kanban-meta))
                     "")
                   (if (and estimate (not (string-empty-p (string-trim estimate))))
                       (propertize (concat " e" (string-trim estimate))
                                   'face '(:foreground "medium purple"))
                     "")))))
    (let ((lw (string-width left)) (rw (string-width right)))
      (if (>= (+ lw rw 1) inner)
          (org-github-dashboard--kanban-fit
           (if (string-empty-p left) right (concat left " " right)) inner)
        (concat left (make-string (- inner lw rw) ?\s) right)))))

(defun org-github-dashboard--kanban-card-lines (card width)
  "Return a list of WIDTH-wide strings rendering CARD including borders."
  (let* ((inner (- width 4))
         (color (or (plist-get card :color) "gray50"))
         (bface `(:foreground ,color))
         (num (plist-get card :number))
         (type (plist-get card :type))
         (raw (replace-regexp-in-string "\\`#[0-9]+ +" "" (or (plist-get card :title) "")))
         (head (concat (propertize (format "#%d" num) 'face 'org-github-dashboard-kanban-number)
                       (if (eq type 'pr)
                           (propertize " ⑂" 'face 'org-github-dashboard-kanban-meta) "")
                       " "
                       (propertize raw 'face 'org-github-dashboard-kanban-title)))
         (tlines (org-github-dashboard--kanban-wrap head inner 2))
         (meta (org-github-dashboard--kanban-meta-line card inner))
         (lb (propertize "│ " 'face bface))
         (rb (propertize " │" 'face bface))
         (top (propertize (concat "┌" (make-string (- width 2) ?─) "┐") 'face bface))
         (bot (propertize (concat "└" (make-string (- width 2) ?─) "┘") 'face bface)))
    (list top
          (concat lb (nth 0 tlines) rb)
          (concat lb (nth 1 tlines) rb)
          (concat lb meta rb)
          bot)))

;;;; Kanban: data collection

(defun org-github-dashboard--kanban-collect ()
  "Collect GitHub items as card plists, honoring the active filters."
  (let ((today (format-time-string "%Y-%m-%d")))
    (org-ql-select (org-agenda-files)
      (org-github-dashboard--filtered-issue-query)
      :action
      (lambda ()
        (let* ((pr (org-entry-get (point) "PR_NUMBER"))
               (numstr (or pr (org-entry-get (point) "ISSUE_NUMBER")))
               (done (org-entry-is-done-p))
               (dl (org-entry-get (point) "DEADLINE"))
               (dldate (and dl (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" dl)
                            (match-string 0 dl)))
               (assignees (org-entry-get (point) "ASSIGNEES")))
          (list :number (string-to-number (or numstr "0"))
                :type (if pr 'pr 'issue)
                :title (org-get-heading t t t t)
                :todo (org-get-todo-state)
                :done done
                :assignee (and assignees (car (split-string assignees "," t " +")))
                :deadline dldate
                :overdue (and dldate (not done) (string< dldate today))
                :estimate (org-entry-get (point) "EFFORT")
                :milestone (org-entry-get (point) "MILESTONE")
                :closed (org-github-dashboard--date-from-ts
                         (org-entry-get (point) "CLOSED_AT"))
                :marker (copy-marker (point))))))))

(defun org-github-dashboard--kanban-visible-p (card cutoff)
  "Non-nil if CARD passes the status and period (CUTOFF) filters."
  (let ((done (plist-get card :done)))
    (and (pcase org-github-dashboard-status
           ('todo (not done))
           ('done done)
           (_ t))
         (or (not done) (null cutoff)
             (let ((cl (plist-get card :closed)))
               (and cl (not (string< cl cutoff))))))))

(defun org-github-dashboard--kanban-card< (a b)
  "Sort comparator: overdue first, then by deadline, then by number."
  (let ((ao (plist-get a :overdue)) (bo (plist-get b :overdue))
        (ad (plist-get a :deadline)) (bd (plist-get b :deadline)))
    (cond ((and ao (not bo)) t)
          ((and bo (not ao)) nil)
          ((and ad bd (not (string= ad bd))) (string< ad bd))
          ((and ad (not bd)) t)
          ((and bd (not ad)) nil)
          (t (< (plist-get a :number) (plist-get b :number))))))

;;;; Kanban: rendering

(defun org-github-dashboard--kanban-title-line (cards)
  "Return the header/help text for the kanban board given visible CARDS."
  (let* ((nopen (seq-count (lambda (c) (not (plist-get c :done))) cards))
         (ndone (seq-count (lambda (c) (plist-get c :done)) cards))
         (filters (string-join
                   (delq nil
                         (list (when org-github-dashboard-repos
                                 (format "repos:%s" (string-join org-github-dashboard-repos ",")))
                               (when org-github-dashboard-assignees
                                 (format "who:%s" (string-join org-github-dashboard-assignees ",")))
                               (when org-github-dashboard-milestones
                                 (format "ms:%s" (string-join org-github-dashboard-milestones ",")))
                               (unless (eq org-github-dashboard-status 'all)
                                 (format "status:%s" org-github-dashboard-status))
                               (when org-github-dashboard-period
                                 (format "period:%s" (car org-github-dashboard-period)))))
                   "  ")))
    (concat (propertize "GitHub Kanban" 'face '(:height 1.2 :weight bold))
            (format "   %d open · %d done" nopen ndone)
            (if (string-empty-p filters) "" (concat "   [" filters "]"))
            "\n"
            (propertize
             "RET open  s sync  M-←/→ move  ←/→/↑/↓ or h/j/k/l navigate  / filter  g refresh  v agenda  q quit"
             'face 'shadow))))

(defun org-github-dashboard--kanban-render ()
  "Render the kanban board into the current buffer."
  (let* ((inhibit-read-only t)
         (columns org-github-dashboard-kanban-columns)
         (ncols (length columns))
         (cutoff (when org-github-dashboard-period
                   (format-time-string
                    "%Y-%m-%d"
                    (time-subtract (current-time)
                                   (days-to-time (cdr org-github-dashboard-period))))))
         (all (seq-filter (lambda (c) (org-github-dashboard--kanban-visible-p c cutoff))
                          (org-github-dashboard--kanban-collect)))
         (gutter 2)
         (gut (make-string gutter ?\s))
         (avail (max 40 (1- (window-width))))
         (width (max org-github-dashboard-kanban-min-card-width
                     (min org-github-dashboard-kanban-max-card-width
                          (/ (- avail (* gutter (1- ncols))) (max 1 ncols)))))
         (col-cards
          (mapcar
           (lambda (col)
             (let* ((kws (nth 1 col))
                    (color (nth 2 col))
                    (cards (sort (seq-filter
                                  (lambda (c) (member (plist-get c :todo) kws)) all)
                                 #'org-github-dashboard--kanban-card<)))
               (dolist (c cards) (plist-put c :color color))
               cards))
           columns))
         (maxrows (apply #'max 0 (mapcar #'length col-cards)))
         (lines-cache (make-hash-table :test 'equal))
         (geoms (make-hash-table :test 'equal))
         (blank (make-string width ?\s)))
    (erase-buffer)
    (setq org-github-dashboard--kanban-grid nil
          org-github-dashboard--kanban-ncols ncols)
    (mapc #'delete-overlay org-github-dashboard--kanban-overlays)
    (setq org-github-dashboard--kanban-overlays nil)
    ;; Title + help
    (insert (org-github-dashboard--kanban-title-line all) "\n\n")
    ;; Column headers
    (dotimes (ci ncols)
      (let* ((col (nth ci columns))
             (text (format " %s (%d)" (upcase (nth 0 col)) (length (nth ci col-cards)))))
        (insert (propertize (org-github-dashboard--kanban-fit text width)
                            'face `(:foreground ,(nth 2 col) :weight bold :underline t)))
        (when (< ci (1- ncols)) (insert gut))))
    (insert "\n")
    ;; Pre-render card lines
    (dotimes (ci ncols)
      (let ((cards (nth ci col-cards)))
        (dotimes (ri (length cards))
          (puthash (cons ci ri)
                   (org-github-dashboard--kanban-card-lines (nth ri cards) width)
                   lines-cache))))
    ;; Lay out cards band by band, capturing each cell's buffer region
    (dotimes (ri maxrows)
      (dotimes (l 5)
        (dotimes (ci ncols)
          (let ((lines (gethash (cons ci ri) lines-cache)))
            (if lines
                (let ((b (point)))
                  (insert (propertize (nth l lines) 'org-github-card (cons ci ri)))
                  (puthash (cons ci ri)
                           (cons (cons b (point)) (gethash (cons ci ri) geoms))
                           geoms))
              (insert blank))
            (when (< ci (1- ncols)) (insert gut))))
        (insert "\n"))
      (insert "\n"))
    ;; Build the navigation grid
    (dotimes (ci ncols)
      (let ((cards (nth ci col-cards)))
        (dotimes (ri (length cards))
          (let ((cells (nreverse (gethash (cons ci ri) geoms))))
            (push (list :col ci :row ri :card (nth ri cards)
                        :cells cells :pos (car (car cells)))
                  org-github-dashboard--kanban-grid)))))
    (setq org-github-dashboard--kanban-grid (nreverse org-github-dashboard--kanban-grid))
    (goto-char (point-min))))

;;;; Kanban: navigation

(defun org-github-dashboard--kanban-entry (col row)
  "Return the grid entry at COL and ROW, or nil."
  (seq-find (lambda (e) (and (= (plist-get e :col) col) (= (plist-get e :row) row)))
            org-github-dashboard--kanban-grid))

(defun org-github-dashboard--kanban-nearest (col row)
  "Return the card entry in COL nearest to ROW, or nil."
  (let ((es (seq-filter (lambda (e) (= (plist-get e :col) col))
                        org-github-dashboard--kanban-grid)))
    (when es
      (car (sort es (lambda (a b)
                      (< (abs (- (plist-get a :row) row))
                         (abs (- (plist-get b :row) row)))))))))

(defun org-github-dashboard--kanban-current ()
  "Return the currently selected grid entry."
  (or (let ((c (get-text-property (point) 'org-github-card)))
        (and c (org-github-dashboard--kanban-entry (car c) (cdr c))))
      org-github-dashboard--kanban-cur
      (car org-github-dashboard--kanban-grid)))

(defun org-github-dashboard--kanban-highlight (entry)
  "Highlight the card ENTRY with overlays."
  (mapc #'delete-overlay org-github-dashboard--kanban-overlays)
  (setq org-github-dashboard--kanban-overlays nil)
  (dolist (cell (plist-get entry :cells))
    (let ((ov (make-overlay (car cell) (cdr cell))))
      (overlay-put ov 'face 'org-github-dashboard-kanban-highlight)
      (overlay-put ov 'priority 100)
      (push ov org-github-dashboard--kanban-overlays))))

(defun org-github-dashboard--kanban-goto (entry)
  "Select grid ENTRY: move point to it and highlight it."
  (when entry
    (setq org-github-dashboard--kanban-cur entry)
    (goto-char (plist-get entry :pos))
    (org-github-dashboard--kanban-highlight entry)))

(defun org-github-dashboard-kanban-right ()
  "Move selection to the nearest card in the next non-empty column."
  (interactive)
  (let* ((cur (org-github-dashboard--kanban-current))
         (col (plist-get cur :col)) (row (plist-get cur :row)) target)
    (cl-loop for c from (1+ col) below org-github-dashboard--kanban-ncols
             do (when (setq target (org-github-dashboard--kanban-nearest c row))
                  (cl-return)))
    (org-github-dashboard--kanban-goto (or target cur))))

(defun org-github-dashboard-kanban-left ()
  "Move selection to the nearest card in the previous non-empty column."
  (interactive)
  (let* ((cur (org-github-dashboard--kanban-current))
         (col (plist-get cur :col)) (row (plist-get cur :row)) target)
    (cl-loop for c from (1- col) downto 0
             do (when (setq target (org-github-dashboard--kanban-nearest c row))
                  (cl-return)))
    (org-github-dashboard--kanban-goto (or target cur))))

(defun org-github-dashboard-kanban-down ()
  "Move selection to the next card down in the current column."
  (interactive)
  (let* ((cur (org-github-dashboard--kanban-current))
         (target (org-github-dashboard--kanban-entry
                  (plist-get cur :col) (1+ (plist-get cur :row)))))
    (when target (org-github-dashboard--kanban-goto target))))

(defun org-github-dashboard-kanban-up ()
  "Move selection to the previous card up in the current column."
  (interactive)
  (let* ((cur (org-github-dashboard--kanban-current))
         (target (org-github-dashboard--kanban-entry
                  (plist-get cur :col) (1- (plist-get cur :row)))))
    (when target (org-github-dashboard--kanban-goto target))))

;;;; Kanban: actions

(defun org-github-dashboard-kanban-open ()
  "Jump to the Org heading for the card at point."
  (interactive)
  (let* ((cur (org-github-dashboard--kanban-current))
         (m (plist-get (plist-get cur :card) :marker)))
    (if (and m (marker-buffer m))
        (progn
          (pop-to-buffer (marker-buffer m))
          (goto-char m)
          (org-back-to-heading t)
          (cond ((fboundp 'org-fold-show-entry) (org-fold-show-entry))
                ((fboundp 'org-show-entry) (org-show-entry))))
      (message "No source location for this card"))))

(defun org-github-dashboard-kanban-sync ()
  "Sync the card at point from GitHub, then refresh the board.
With a prefix argument, force-pull; with two, force-push."
  (interactive)
  (let* ((cur (org-github-dashboard--kanban-current))
         (m (plist-get (plist-get cur :card) :marker))
         (buf (current-buffer)))
    (if (and m (marker-buffer m))
        (with-current-buffer (marker-buffer m)
          (save-excursion
            (goto-char m)
            (org-github-sync-at-point-async
             current-prefix-arg
             (lambda (_err)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (org-github-dashboard-kanban-refresh)))))))
      (message "No source location for this card"))))

(defun org-github-dashboard-kanban-move (dir)
  "Move the current card to the adjacent column (DIR -1 left, +1 right).
Sets the item's Org TODO keyword to that column's first keyword.  When
`org-github-mode' is active, that state change propagates to GitHub."
  (let* ((cur (org-github-dashboard--kanban-current))
         (target-col (+ (plist-get cur :col) dir))
         (card (plist-get cur :card))
         (m (plist-get card :marker)))
    (cond
     ((or (< target-col 0) (>= target-col org-github-dashboard--kanban-ncols))
      (message "No column in that direction"))
     ((not (and m (marker-buffer m))) (message "No source location for this card"))
     (t
      (let ((kw (car (nth 1 (nth target-col org-github-dashboard-kanban-columns)))))
        (with-current-buffer (marker-buffer m)
          (save-excursion
            (goto-char m)
            (let ((org-blocker-hook nil))
              (org-todo kw))
            (save-buffer)))
        (message "Moved #%d → %s" (plist-get card :number)
                 (nth 0 (nth target-col org-github-dashboard-kanban-columns)))
        (org-github-dashboard-kanban-refresh))))))

(defun org-github-dashboard-kanban-move-right ()
  "Move the current card one column to the right."
  (interactive) (org-github-dashboard-kanban-move 1))

(defun org-github-dashboard-kanban-move-left ()
  "Move the current card one column to the left."
  (interactive) (org-github-dashboard-kanban-move -1))

(defun org-github-dashboard-kanban-refresh ()
  "Re-render the kanban board, preserving the selected card if possible."
  (interactive)
  (let ((num (and org-github-dashboard--kanban-cur
                  (plist-get (plist-get org-github-dashboard--kanban-cur :card) :number))))
    (org-github-dashboard--kanban-render)
    (org-github-dashboard--kanban-goto
     (or (and num (seq-find (lambda (e)
                              (= (plist-get (plist-get e :card) :number) num))
                            org-github-dashboard--kanban-grid))
         (car org-github-dashboard--kanban-grid)))))

;;;; Kanban: mode and entry point

(defvar org-github-dashboard-kanban-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")       #'org-github-dashboard-kanban-open)
    (define-key map (kbd "<right>")   #'org-github-dashboard-kanban-right)
    (define-key map (kbd "<left>")    #'org-github-dashboard-kanban-left)
    (define-key map (kbd "<up>")      #'org-github-dashboard-kanban-up)
    (define-key map (kbd "<down>")    #'org-github-dashboard-kanban-down)
    (define-key map "l" #'org-github-dashboard-kanban-right)
    (define-key map "h" #'org-github-dashboard-kanban-left)
    (define-key map "j" #'org-github-dashboard-kanban-down)
    (define-key map "k" #'org-github-dashboard-kanban-up)
    (define-key map "n" #'org-github-dashboard-kanban-down)
    (define-key map "p" #'org-github-dashboard-kanban-up)
    (define-key map (kbd "M-<right>") #'org-github-dashboard-kanban-move-right)
    (define-key map (kbd "M-<left>")  #'org-github-dashboard-kanban-move-left)
    (define-key map "L" #'org-github-dashboard-kanban-move-right)
    (define-key map "H" #'org-github-dashboard-kanban-move-left)
    (define-key map "s" #'org-github-dashboard-kanban-sync)
    (define-key map "g" #'org-github-dashboard-kanban-refresh)
    (define-key map "/" #'org-github-dashboard-toggle-filter)
    (define-key map "v" #'org-github-dashboard)
    (define-key map "a" #'org-github-dashboard)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `org-github-dashboard-kanban-mode'.")

(define-derived-mode org-github-dashboard-kanban-mode special-mode "GH-Kanban"
  "Major mode for the org-github horizontal kanban board."
  (setq truncate-lines t)
  (setq-local cursor-type nil)
  (buffer-disable-undo))

;;;###autoload
(defun org-github-dashboard-kanban ()
  "Show GitHub issues/PRs as a horizontal kanban board, by Org TODO state.
Columns come from `org-github-dashboard-kanban-columns' and honor the
same repo/assignee/milestone/status/period filters as the agenda
dashboard (press \\`/' to change them)."
  (interactive)
  (let ((buf (get-buffer-create "*GitHub Kanban*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-github-dashboard-kanban-mode)
        (org-github-dashboard-kanban-mode))
      (org-github-dashboard--kanban-render)
      (when org-github-dashboard--kanban-grid
        (org-github-dashboard--kanban-goto (car org-github-dashboard--kanban-grid))))
    (pop-to-buffer buf)
    (delete-other-windows)))

(provide 'org-github-dashboard)

;;; org-github-dashboard.el ends here
