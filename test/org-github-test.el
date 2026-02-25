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

(provide 'org-github-test)

;;; org-github-test.el ends here
