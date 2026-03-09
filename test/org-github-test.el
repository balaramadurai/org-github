;;; org-github-test.el --- Tests for org-github -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for org-github pure/deterministic functions.

;;; Code:

(require 'ert)
(require 'org-github)

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

(provide 'org-github-test)

;;; org-github-test.el ends here
