;;; gdocs-sync-status-test.el --- Static status orchestration contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline push orchestration and callback context, not live API conformance.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gdocs-sync)
(require 'gdocs-convert)
(require 'gdocs-test-helpers)

(ert-deftest gdocs-sync-test-status-conflicts-do-not-fall-back-to-text-merge ()
  "A static status is semantic structure even in an otherwise plain paragraph."
  (with-temp-buffer
    (org-mode)
    (insert "Local [[status:Green][Ready]].\n")
    (should-error
     (gdocs-sync--abort-unsafe-semantic-merge-if-needed
      (gdocs-convert-org-string-to-ir "Remote [[status:Red][Ready]].\n"))
     :type 'user-error)))

(ert-deftest gdocs-sync-test-status-push-preserves-callback-source-and-provider-ids ()
  "Initial and existing-document pushes use captured source/account state."
  (dolist (existing '(nil t))
    (let* ((source (generate-new-buffer " *status-push-source*"))
           (other (generate-new-buffer " *status-push-callback*"))
           (directory (make-temp-file "gdocs-status-push-" t))
           (gdocs-directory directory)
           (gdocs-link-directories nil)
           (gdocs-preserve-comments nil)
           fetch-callback sent-requests
           (fetches 0) (writes 0)
           (json (json-parse-string
                  (if existing
                      (concat "{\"body\":{\"content\":[{\"startIndex\":1,\"endIndex\":20,"
                              "\"paragraph\":{\"elements\":[{\"startIndex\":1,\"endIndex\":20,"
                              "\"textRun\":{\"content\":\"Keep Ready anchor.\\n\"}}]}}]},"
                              "\"namedRanges\":{\"gdocs-org-marker:status:run-6:status:Green:1\":{"
                              "\"namedRanges\":[{\"namedRangeId\":\"old-marker\",\"ranges\":[{\"startIndex\":6,\"endIndex\":11}]}]}}}")
                    "{\"body\":{\"content\":[{\"startIndex\":1,\"endIndex\":2,\"paragraph\":{\"elements\":[{\"textRun\":{\"content\":\"\\n\"}}]}}]}}")
                  :object-type 'alist)))
      (unwind-protect
          (cl-letf (((symbol-function 'gdocs-api-get-document)
                     (lambda (id callback &optional account _error)
                       (should (equal id "status-doc"))
                       (should (equal account "status-account"))
                       (cl-incf fetches)
                       (setq fetch-callback callback)))
                    ((symbol-function 'gdocs-api-batch-update)
                     (lambda (id requests callback &optional account _error)
                       (should (eq (current-buffer) source))
                       (should (equal id "status-doc"))
                       (should (equal account "status-account"))
                       (cl-incf writes)
                       (setq sent-requests requests)
                       (funcall callback '((writeControl . ((requiredRevisionId . "status-r2"))))))))
            (with-current-buffer source
              (org-mode)
              (insert "Keep [[status:Red][Ready]] anchor.\n")
              (setq-local gdocs-sync--document-id "status-doc")
              (setq-local gdocs-sync--account "status-account")
              (gdocs-sync-push)
              (should (= writes 0)))
            (with-current-buffer other (funcall fetch-callback json))
            (should (= writes 1))
            (should (seq-some (lambda (request) (alist-get 'createNamedRange request)) sent-requests))
            (when existing
              (should (equal (car sent-requests)
                             '((deleteNamedRange . ((namedRangeId . "old-marker")))))))
            (with-current-buffer source
              (should (eq gdocs-sync--status 'synced))
              (should (equal gdocs-sync--revision-id "status-r2"))
              (should-not gdocs-sync--push-in-progress)
              (gdocs-sync-push))
            (should (= fetches 1))
            (should (= writes 1)))
        (dolist (buffer (list source other))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))
        (delete-directory directory t)))))

(provide 'gdocs-sync-status-test)
;;; gdocs-sync-status-test.el ends here
