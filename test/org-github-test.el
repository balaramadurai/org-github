;;; org-github-test.el --- Tests for org-github -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for org-github pure/deterministic functions.

;;; Code:

(require 'ert)
(require 'org-github)
(require 'cl-lib)

(defconst org-github-test--dir
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name))))
  "Root directory of the org-github source tree.")

;;; State-to-TODO conversion

(ert-deftest org-github-test-state-to-todo ()
  "Test GitHub state to Org TODO keyword conversion."
  ;; Issues
  (should (string= (org-github--state-to-todo "OPEN" 'issue) "TODO"))
  (should (string= (org-github--state-to-todo "open" 'issue) "TODO"))
  (should (string= (org-github--state-to-todo "CLOSED" 'issue) "DONE"))
  ;; PRs
  (should (string= (org-github--state-to-todo "OPEN" 'pr) "TODO"))
  (should (string= (org-github--state-to-todo "CLOSED" 'pr) "CANCELLED"))
  (should (string= (org-github--state-to-todo "MERGED" 'pr) "DONE")))

;;; Tag sanitization

(ert-deftest org-github-test-sanitize-tag ()
  "Test GitHub label sanitization for Org tags."
  (should (string= (org-github--sanitize-tag "bug") "bug"))
  (should (string= (org-github--sanitize-tag "good first issue") "good_first_issue"))
  (should (string= (org-github--sanitize-tag "type-bug") "type_bug"))
  (should (string= (org-github--sanitize-tag "scope:frontend") "scope_frontend")))

;;; JSON parsing

(ert-deftest org-github-test-parse-json ()
  "Test JSON parsing."
  (let ((result (org-github--parse-json "{\"number\": 42, \"state\": \"open\"}")))
    (should (= (alist-get 'number result) 42))
    (should (string= (alist-get 'state result) "open")))
  ;; Invalid JSON returns nil
  (should (null (org-github--parse-json "not json")))
  ;; Empty string returns nil
  (should (null (org-github--parse-json ""))))

;;; Time formatting

(ert-deftest org-github-test-format-time ()
  "Test active Org timestamp formatting."
  ;; Valid ISO timestamp produces active timestamp
  (let ((result (org-github--format-time "2025-01-15T10:30:00Z")))
    (should (string-match-p "^<2025-01-15" result))
    (should (string-match-p ">$" result)))
  ;; nil returns nil
  (should (null (org-github--format-time nil)))
  ;; Empty string returns nil
  (should (null (org-github--format-time ""))))

(ert-deftest org-github-test-format-time-plain ()
  "Test inactive Org timestamp formatting."
  (let ((result (org-github--format-time-plain "2025-01-15T10:30:00Z")))
    (should (string-match-p "^\\[2025-01-15" result))
    (should (string-match-p "\\]$" result)))
  (should (null (org-github--format-time-plain nil)))
  (should (null (org-github--format-time-plain ""))))

;;; Body-parsed deadline

(ert-deftest org-github-test-parse-body-deadline ()
  "Test parsing \"Due:\"/\"Deadline:\" dates out of issue/PR body text."
  ;; Basic matches, case-insensitive, with/without space after colon
  (should (string= (org-github--parse-body-deadline "Due: 2026-09-15") "2026-09-15"))
  (should (string= (org-github--parse-body-deadline "deadline: 2026-01-03") "2026-01-03"))
  (should (string= (org-github--parse-body-deadline "Due:2026-09-15") "2026-09-15"))
  ;; Matches anywhere in a multi-line body, with leading whitespace
  (should (string= (org-github--parse-body-deadline "Some notes.\n  Deadline: 2026-12-25\nMore text.")
                    "2026-12-25"))
  ;; No match returns nil
  (should (null (org-github--parse-body-deadline "No date here.")))
  (should (null (org-github--parse-body-deadline "Due: not-a-date")))
  ;; nil body returns nil
  (should (null (org-github--parse-body-deadline nil))))

;;; Deadline resolution

(ert-deftest org-github-test-resolve-deadline ()
  "Test deadline resolution: issue body deadline wins over Project V2."
  ;; Both nil → nil
  (should (null (org-github--resolve-deadline nil nil)))
  ;; Only body → body
  (should (string= (org-github--resolve-deadline "2026-09-15" nil) "2026-09-15"))
  ;; Only project → project
  (should (string= (org-github--resolve-deadline nil "2026-10-01") "2026-10-01"))
  ;; Both present, different dates → body wins (issue-level priority)
  (should (string= (org-github--resolve-deadline "2026-09-15" "2026-10-01") "2026-09-15"))
  ;; Both present, same date → either (body returned)
  (should (string= (org-github--resolve-deadline "2026-10-01" "2026-10-01") "2026-10-01")))

;;; Issue-to-Org conversion

(ert-deftest org-github-test-issue-to-org ()
  "Test issue to Org heading conversion."
  (let* ((issue '((number . 42)
                  (title . "Fix login bug")
                  (body . "The login is broken")
                  (state . "OPEN")
                  (url . "https://github.com/owner/repo/issues/42")
                  (createdAt . "2025-01-15T10:30:00Z")
                  (updatedAt . "2025-01-20T14:22:00Z")
                  (closedAt)
                  (author . ((login . "testuser")))
                  (labels . (((name . "bug")) ((name . "enhancement"))))
                  (assignees . (((login . "dev1")) ((login . "dev2"))))
                  (milestone . ((title . "v1.0")))))
         (result (org-github--issue-to-org issue "owner/repo")))
    ;; Heading format
    (should (string-match-p "^\\*\\*\\* TODO #42 Fix login bug" result))
    ;; Tags
    (should (string-match-p ":bug:enhancement:" result))
    ;; Properties
    (should (string-match-p ":ISSUE_NUMBER: 42" result))
    (should (string-match-p ":REPO: owner/repo" result))
    (should (string-match-p ":STATE: OPEN" result))
    (should (string-match-p ":AUTHOR: testuser" result))
    (should (string-match-p ":ASSIGNEES: dev1, dev2" result))
    (should (string-match-p ":MILESTONE: v1.0" result))
    ;; Body
    (should (string-match-p "The login is broken" result))))

(ert-deftest org-github-test-issue-to-org-planning-line ()
  "DEADLINE and SCHEDULED must land on ONE combined Org planning line
directly under the heading -- Org only parses a single planning line, so
a standalone second \"SCHEDULED:\" line would just become body text."
  (let ((base-issue '((number . 42)
                      (title . "Fix login bug")
                      (body . "")
                      (state . "OPEN")
                      (url . "https://github.com/owner/repo/issues/42")
                      (createdAt . "2025-01-15T10:30:00Z")
                      (updatedAt . "2025-01-20T14:22:00Z")
                      (closedAt))))
    ;; Both deadline and scheduled -> single combined planning line
    (let* ((issue (append '((deadline . "2026-09-15") (scheduled . "2026-09-01")) base-issue))
           (result (org-github--issue-to-org issue "owner/repo"))
           (planning-line (nth 1 (split-string result "\n"))))
      (should (string-match-p "^DEADLINE: <2026-09-15" planning-line))
      (should (string-match-p "SCHEDULED: <2026-09-01" planning-line))
      (should (string-suffix-p ">" planning-line)))
    ;; Scheduled only (no deadline) -> just a SCHEDULED planning line
    (let* ((issue (append '((scheduled . "2026-09-01")) base-issue))
           (result (org-github--issue-to-org issue "owner/repo"))
           (planning-line (nth 1 (split-string result "\n"))))
      (should (string-match-p "^SCHEDULED: <2026-09-01" planning-line))
      (should-not (string-match-p "DEADLINE:" planning-line)))
    ;; Neither present -> no planning line at all (existing behavior)
    (let ((result (org-github--issue-to-org base-issue "owner/repo")))
      (should-not (string-match-p "DEADLINE:\\|SCHEDULED:" result))
      (should (string-match-p "\\*\\*\\* TODO #42 Fix login bug\n:PROPERTIES:" result)))))

;;; Gantt chart tests

;; Load dashboard pure functions without triggering org-ql dependency.
;; We eval the specific function definitions from the source file.
(let ((dashboard-file (expand-file-name "org-github-dashboard.el"
                                         (file-name-directory
                                          (directory-file-name
                                           (file-name-directory
                                            (or load-file-name buffer-file-name)))))))
  (with-temp-buffer
    (insert-file-contents dashboard-file)
    ;; Eval defcustom and defun forms for the pure functions we test
    (goto-char (point-min))
    (while (re-search-forward
            (rx bol "(" (or "defun" "defcustom") " org-github-dashboard-"
                (or "-gantt-task-id"
                    "-format-gantt"
                    "-format-investor-report"
                    "report-title"
                    "report-include-gantt"))
            nil t)
      (goto-char (match-beginning 0))
      (let ((form (read (current-buffer))))
        (eval form t)))))

(ert-deftest org-github-test-gantt-task-id ()
  "Test Mermaid task ID sanitization."
  (should (string= (org-github-dashboard--gantt-task-id "owner/repo" "issue" "42")
                    "owner_repo_issue_42"))
  (should (string= (org-github-dashboard--gantt-task-id "my-org/my-repo" "pr" "7")
                    "my_org_my_repo_pr_7"))
  (should (string= (org-github-dashboard--gantt-task-id "a/b" "issue" "1")
                    "a_b_issue_1")))

(ert-deftest org-github-test-format-gantt ()
  "Test Mermaid Gantt chart formatting."
  (let ((items (list (list :title "Fix login" :repo "owner/repo" :type "issue"
                           :number "1" :state "done" :created "2026-01-01"
                           :deadline "2026-01-15" :assignee "dev1")
                     (list :title "Add feature" :repo "owner/repo" :type "pr"
                           :number "2" :state "active" :created "2026-01-10"
                           :deadline "2026-02-01" :assignee "dev2"))))
    (let ((result (org-github-dashboard--format-gantt items)))
      (should (string-match-p "gantt" result))
      (should (string-match-p "dateFormat YYYY-MM-DD" result))
      (should (string-match-p "section owner/repo" result))
      (should (string-match-p "Fix login" result))
      (should (string-match-p "done" result))
      (should (string-match-p "active" result))
      (should (string-match-p "2026-01-01" result))
      (should (string-match-p "2026-02-01" result)))))

(ert-deftest org-github-test-format-gantt-critical ()
  "Test that overdue items get crit keyword in Gantt chart."
  (let ((items (list (list :title "Overdue task" :repo "owner/repo" :type "issue"
                           :number "5" :state "crit" :created "2026-01-01"
                           :deadline "2026-01-10" :assignee "dev1"))))
    (let ((result (org-github-dashboard--format-gantt items)))
      (should (string-match-p "crit" result))
      (should (string-match-p "Overdue task" result)))))

(ert-deftest org-github-test-format-gantt-empty ()
  "Test Gantt chart with empty input produces header only."
  (let ((result (org-github-dashboard--format-gantt '())))
    (should (string-match-p "gantt" result))
    (should (string-match-p "dateFormat" result))
    (should-not (string-match-p "section" result))))

(ert-deftest org-github-test-format-investor-report ()
  "Test investor report formatting with mocked data."
  (let ((org-github-dashboard-report-title "Test Report")
        (org-github-dashboard-report-include-gantt t)
        (repo-stats '(("org/repo1" . (:open-issues 5 :open-prs 2 :done-issues 10 :done-prs 3))))
        (assignee-stats '(("alice" . (:open-issues 3 :open-prs 1 :done-issues 5 :done-prs 2))))
        (overdue '((:title "Late task" :repo "org/repo1" :type "Issue"
                    :number "10" :deadline "2026-01-01" :assignees "alice")))
        (upcoming '((:title "Soon task" :repo "org/repo1" :type "PR"
                     :number "20" :deadline "2026-03-01" :assignees "bob")))
        (gantt-items '((:title "Task" :repo "org/repo1" :type "issue"
                        :number "1" :state "active" :created "2026-01-01"
                        :deadline "2026-02-01" :assignee "alice"))))
    (let ((result (org-github-dashboard--format-investor-report
                   repo-stats assignee-stats overdue upcoming gantt-items)))
      ;; Check all sections exist
      (should (string-match-p "Executive Summary" result))
      (should (string-match-p "Repository Breakdown" result))
      (should (string-match-p "Team Workload" result))
      (should (string-match-p "Overdue Items" result))
      (should (string-match-p "Upcoming Deadlines" result))
      (should (string-match-p "Project Timeline" result))
      ;; Check data appears
      (should (string-match-p "Test Report" result))
      (should (string-match-p "org/repo1" result))
      (should (string-match-p "alice" result))
      (should (string-match-p "Late task" result))
      (should (string-match-p "Soon task" result))
      ;; Check Gantt is embedded
      (should (string-match-p "mermaid" result))
      (should (string-match-p "gantt" result))
      ;; Check metrics
      (should (string-match-p "Total Items" result))
      (should (string-match-p "Completion Rate" result)))))

;;; Sync suppression

(ert-deftest org-github-test-syncing-is-special ()
  "`org-github--syncing' must be a special (dynamically bound) variable.
The bulk sync commands bind it with `let' to suppress
`org-github--on-todo-state-change'.  Under `lexical-binding', a `let'
that the byte-compiler sees before the `defvar' compiles to a plain
lexical binding, so the hook still fires and the closing-note prompt
blocks every bulk sync."
  (should (special-variable-p 'org-github--syncing)))

(ert-deftest org-github-test-syncing-defvar-precedes-first-binding ()
  "The `defvar' for `org-github--syncing' must come before any `let' of it."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "org-github.el" org-github-test--dir))
    (goto-char (point-min))
    (let ((defvar-pos (progn (re-search-forward "^(defvar org-github--syncing" nil t)
                             (match-beginning 0))))
      (should defvar-pos)
      (goto-char (point-min))
      (should (re-search-forward "org-github--syncing" nil t))
      (should (>= (match-beginning 0) defvar-pos)))))

(ert-deftest org-github-test-read-closing-note ()
  "The closing-note prompt is opt-out and treats whitespace as empty."
  (let ((org-github-prompt-closing-note nil))
    (should (null (org-github--read-closing-note "42"))))
  (let ((org-github-prompt-closing-note t))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "   ")))
      (should (null (org-github--read-closing-note "42"))))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) " shipped ")))
      (should (equal (org-github--read-closing-note "42") "shipped")))))

(provide 'org-github-test)

;;; org-github-test.el ends here
