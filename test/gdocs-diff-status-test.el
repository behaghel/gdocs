;;; gdocs-diff-status-test.el --- Static status diff contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; Atomic status diffing and provider marker lifecycle are tested separately.

;;; Code:

(require 'ert)
(require 'gdocs-diff)

(ert-deftest gdocs-status-diff-has-a-dedicated-module ()
  "Status tokenization and provider marker cleanup live outside the dispatcher."
  (dolist (function '(gdocs-diff--status-marker-delete-requests
                      gdocs-diff--tokenize-status-runs))
    (should (equal (file-name-base (symbol-file function 'defun))
                   "gdocs-diff-status"))))

(ert-deftest gdocs-diff-test-status-colour-change-is-not-a-noop ()
  "Changing colour updates marker/style without deleting text or comment anchors."
  (let* ((old (let ((ir (gdocs-convert-org-string-to-ir "Keep [[status:Green][Ready]] anchor.\n")))
                (plist-put (seq-find (lambda (run) (plist-get run :status))
                                     (plist-get (car ir) :contents))
                           :status-marker-ids '("old-status-1"))
                ir))
         (new (gdocs-convert-org-string-to-ir "Keep [[status:Red][Ready]] anchor.\n"))
         (requests (gdocs-diff-generate old new))
         (deletion (seq-some (lambda (request) (alist-get 'deleteContentRange request)) requests))
         (marker (seq-some (lambda (request) (alist-get 'createNamedRange request)) requests)))
    (should requests)
    (should (equal (car requests) '((deleteNamedRange . ((namedRangeId . "old-status-1"))))))
    (should-not deletion)
    (should-not (seq-some (lambda (request) (alist-get 'insertText request)) requests))
    (should marker)
    (should (string-suffix-p ":status:Red:1" (alist-get 'name marker)))
    (should-not (gdocs-diff-generate new new))))

(ert-deftest gdocs-diff-test-status-surrounding-edit-keeps-pill-and-anchor ()
  "Ordinary edits do not delete an unchanged pill or duplicate its metadata."
  (let* ((old (gdocs-convert-org-string-to-ir "Keep [[status:Green][Ready]] anchor.\n"))
         (new (gdocs-convert-org-string-to-ir "Edit [[status:Green][Ready]] anchor.\n"))
         (requests (gdocs-diff-generate old new))
         (deletion (seq-some (lambda (request) (alist-get 'deleteContentRange request)) requests)))
    (should (equal (alist-get 'range deletion) '((startIndex . 1) (endIndex . 5) (segmentId . ""))))
    (should-not (seq-some (lambda (request) (alist-get 'createNamedRange request)) requests))))

(ert-deftest gdocs-diff-test-status-removal-clears-generated-background ()
  "Replacing a pill by plain text clears generated formatting and stale links."
  (let* ((old (gdocs-convert-org-string-to-ir "Keep [[status:Green][Ready]] anchor.\n"))
         (new (gdocs-convert-org-string-to-ir "Keep Ready anchor.\n"))
         (requests (gdocs-diff-generate old new)))
    (should requests)
    (should (seq-some
             (lambda (request)
               (let ((update (alist-get 'updateTextStyle request)))
                 (and (string-match-p "backgroundColor" (or (alist-get 'fields update) ""))
                      (string-match-p "link" (or (alist-get 'fields update) ""))
                      (not (alist-get 'backgroundColor (alist-get 'textStyle update))))))
             requests))))

(ert-deftest gdocs-diff-test-status-boundary-edit-does-not-rewrite-identical-text ()
  "Moving status meaning within identical visible text changes ranges, not text."
  (let* ((old (gdocs-convert-org-string-to-ir "A [[status:Green][Ready]] Ready.\n"))
         (new (gdocs-convert-org-string-to-ir "A Ready [[status:Green][Ready]].\n")))
    (plist-put (seq-find (lambda (run) (plist-get run :status)) (plist-get (car old) :contents))
               :status-marker-ids '("old-boundary"))
    (let* ((requests (gdocs-diff-generate old new))
           (marker (seq-some (lambda (request) (alist-get 'createNamedRange request)) requests)))
      (should (equal (car requests) '((deleteNamedRange . ((namedRangeId . "old-boundary"))))))
      (should (equal (alist-get 'range marker) '((startIndex . 9) (endIndex . 14))))
      (should-not (seq-some (lambda (request)
                              (or (alist-get 'insertText request) (alist-get 'deleteContentRange request)))
                            requests)))))

(ert-deftest gdocs-diff-test-status-paragraph-deletion-cleans-provider-marker-first ()
  "Deleting a status paragraph removes its provider metadata before its text."
  (let ((old (gdocs-convert-org-string-to-ir "[[status:Green][Ready]]\n")))
    (plist-put (car (plist-get (car old) :contents)) :status-marker-ids '("deleted-status"))
    (let ((requests (gdocs-diff-generate old nil)))
      (should (equal (car requests) '((deleteNamedRange . ((namedRangeId . "deleted-status"))))))
      (should (alist-get 'deleteContentRange (cadr requests))))))

(provide 'gdocs-diff-status-test)
;;; gdocs-diff-status-test.el ends here
