;;; gdocs-pull-preservation-test.el --- Pull preservation regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Recover the behavior identified on c6162f2 without its mtime heuristic.
;; Real conversion, validation, backup and replacement run against fake API data.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'gdocs-sync)
(require 'gdocs-test-helpers)

(defconst gdocs-pull-preservation-test--local
  (concat ":PROPERTIES:\n:GDOCS_DOCUMENT_ID: test-doc-id\n"
          ":GDOCS_ACCOUNT: test-account\n:END:\n"
          "#+title: My Doc\n#+date: [2026-07-29 Wed]\n"
          "#+filetags: :business:\n#+identifier: 20260729T082937\n\nOld local content\n")
  "Synthetic local document with ordinary user-owned metadata.")

(defconst gdocs-pull-preservation-test--remote
  '((title . "My Doc")
    (body . ((content . [((startIndex . 0) (endIndex . 1) (sectionBreak . t))
                         ((startIndex . 1) (endIndex . 16)
                          (paragraph . ((elements . [((startIndex . 1) (endIndex . 16)
                                                      (textRun . ((content . "Remote content\n"))))])
                                        (paragraphStyle . ((namedStyleType . "NORMAL_TEXT"))))))]))))
  "Representative document JSON; no transport or credentials are used.")

(ert-deftest gdocs-sync-test-pull-preserves-local-denote-frontmatter ()
  "Real first-pull and unchanged-shadow replacement preserve local preamble."
  (dolist (shadow '(nil t))
    (let ((directory (make-temp-file "gdocs-preamble-backup-" t)))
      (unwind-protect
          (with-temp-buffer
            (org-mode)
            (insert gdocs-pull-preservation-test--local)
            (setq-local gdocs-sync--document-id "test-doc-id"
                        gdocs-sync--account "test-account"
                        gdocs-sync--revision-id "old-revision")
            (when shadow
              (setq gdocs-sync--shadow-ir (gdocs-convert-org-buffer-to-ir)
                    gdocs-sync--local-hash (gdocs-sync--current-local-hash)))
            (let ((gdocs-pull-backup-directory directory)
                  (gdocs-pull-integrity-checks-enable t))
              (cl-letf (((symbol-function 'gdocs-api-get-file-metadata)
                         (lambda (_id callback &optional _account)
                           (funcall callback '((headRevisionId . "new-revision")))))
                        ((symbol-function 'gdocs-api-get-document)
                         (lambda (_id callback &optional _account _on-error)
                           (funcall callback gdocs-pull-preservation-test--remote))))
                (gdocs-sync-pull))
              (let ((content (buffer-string)) (case-fold-search nil))
                (dolist (line '("#+title: My Doc" "#+date: [2026-07-29 Wed]"
                                "#+filetags: :business:" "#+identifier: 20260729T082937"))
                  (should (string-match-p (concat "^" (regexp-quote line) "$") content)))
                (should-not (string-match-p "^#\\+TITLE:" content))
                (should (string-match-p "Remote content" content))
                (should-not (string-match-p "Old local content" content)))
              (should (gdocs-sync--local-hash-matches-p))
              (let ((backups (directory-files directory t "gdocs-pull-backup")))
                (should (= (length backups) 1))
                (should (equal (with-temp-buffer
                                 (insert-file-contents (car backups)) (buffer-string))
                               gdocs-pull-preservation-test--local)))))
        (delete-directory directory t)))))

(ert-deftest gdocs-sync-test-list-structure-allows-remote-additions ()
  "Remote list additions pass real semantic validation without disabling it."
  (let ((old "- Existing one\n- Existing two\n")
        (new "- Existing one\n  - Added nested item\n- Existing two\n")
        (gdocs-pull-integrity-checks-enable t))
    (with-temp-buffer
      (org-mode) (insert old)
      (gdocs-sync--validate-pull-replacement new))))

(ert-deftest gdocs-sync-test-list-additions-do-not-mask-lost-nesting ()
  "New siblings cannot mask disappearance of an existing nested structure."
  (let ((gdocs-pull-integrity-checks-enable t))
    (with-temp-buffer
      (org-mode) (insert "- Existing\n  - Nested\n")
      (should-error (gdocs-sync--validate-pull-replacement "- Existing\n- Nested\n- Added\n")
                    :type 'user-error))))

(ert-deftest gdocs-sync-test-preservation-parsing-does-not-run-user-hooks ()
  "Boundary parsing and integrity inventories are data operations, not sessions."
  (let ((calls 0)
        (specimen (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "fixtures/kitchen-sink.org"
                                       (file-name-directory (locate-library "gdocs-sync-test"))))
                    (buffer-string))))
    (let ((org-mode-hook (list (lambda () (setq calls (1+ calls))))))
      (gdocs-preamble-split specimen)
      (should-not (gdocs-sync--pull-integrity-diagnostics specimen specimen))
      (should (= calls 0)))))

(ert-deftest gdocs-sync-test-local-saved-edits-never-trust-file-mtime ()
  "Matching sync time and mtime cannot turn a real local edit into unchanged."
  (let ((file (make-temp-file "gdocs-edited-local-" nil ".org" "Edited body\n")))
    (unwind-protect
        (with-temp-buffer
          (org-mode) (insert "Edited body\n")
          (setq buffer-file-name file
                gdocs-sync--last-sync-time (format-time-string "%FT%T%z")
                gdocs-sync--local-hash (secure-hash 'sha256 "Original body\n")
                gdocs-sync--shadow-ir (gdocs-convert-org-string-to-ir "Original body\n"))
          (set-buffer-modified-p nil)
          (should-not (gdocs-sync--local-unchanged-p)))
      (delete-file file))))

(ert-deftest gdocs-pull-preservation-module-boundaries ()
  "Semantic validation belongs to its module, not the sync orchestrator."
  (dolist (function '(gdocs-sync--org-semantic-inventory gdocs-sync--list-structure-diagnostics
                                                         gdocs-sync--pull-integrity-diagnostics gdocs-sync--semantic-ir-p))
    (should (string-match-p "gdocs-sync-safety\\.elc?\\'" (symbol-file function 'defun))))
  (let ((directory (file-name-directory (locate-library "gdocs-sync"))))
    (with-temp-buffer
      (should (zerop
               (call-process
                (expand-file-name invocation-name invocation-directory) nil t nil
                "--batch" "-Q" "-L" directory
                "--eval" "(progn (require 'gdocs-preamble) (require 'gdocs-sync-safety) (dolist (feature '(gdocs-sync gdocs-convert gdocs-api gdocs-auth)) (when (featurep feature) (error \"Data modules loaded %s\" feature))) (gdocs-preamble-preserve \"#+title: Local\\n\\nOld\\n\" \"#+TITLE: Remote\\n\\nNew\\n\"))"))))))

(ert-deftest gdocs-preamble-preserves-body-affiliations-and-empty-local-metadata ()
  "No metadata is invented, and body-owned captions are not stripped."
  (let ((remote "#+TITLE: Remote\n\n#+CAPTION: Figure\n[[file:figure.png]]\n"))
    (should (equal (gdocs-preamble-preserve "Local body\n" remote) remote))
    (should (equal (gdocs-preamble-preserve "\n\nLocal body\n" remote) remote))
    (should (equal (gdocs-preamble-preserve "#+title: Local\n\nOld\n" remote)
                   "#+title: Local\n\n#+CAPTION: Figure\n[[file:figure.png]]\n"))))

(provide 'gdocs-pull-preservation-test)
;;; gdocs-pull-preservation-test.el ends here
