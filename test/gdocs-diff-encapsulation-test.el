;;; gdocs-diff-encapsulation-test.el --- Semantic boundary contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; The kernel knows generic IR/index/LCS mechanics.  Feature modules supply
;; semantic meaning through the same narrow protocol, including synthetic kinds.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'gdocs-diff)

(ert-deftest gdocs-diff-kernel-has-no-inline-feature-policy ()
  "Footnote and status policy belong outside the generic engine."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "gdocs-diff.el"
                       (file-name-directory (locate-library "gdocs-diff"))))
    (goto-char (point-min))
    (cl-labels ((inspect (form)
                  (cond
                   ((symbolp form)
                    (should-not (string-match-p "footnote\\|status" (symbol-name form))))
                   ((consp form) (inspect (car form)) (inspect (cdr form)))
                   ((vectorp form) (mapc #'inspect form)))))
      (condition-case nil
          (while t (inspect (read (current-buffer))))
        (end-of-file nil)))))

(ert-deftest gdocs-diff-semantic-contract-accepts-an-unrelated-kind ()
  "Element identity, body length, runs and planning do not hardcode a feature."
  (let ((gdocs-diff-semantic-element-functions
         (list (lambda (element)
                 (when (eq (plist-get element :type) 'specimen)
                   '(:key "specimen:stable" :body-length 0)))))
        (gdocs-diff-semantic-run-functions
         (list (lambda (run)
                 (when (plist-get run :specimen)
                   (list :text "@specimen" :key "X" :normalize-whitespace t
                         :requests (lambda (_run index)
                                     (list (list (cons 'specimenIndex index)))))))))
        (gdocs-diff-semantic-plan-functions
         (list (lambda (_old _new operations index)
                 (list :operations operations
                       :before (list (list (cons 'specimenPlan index))))))))
    (should (equal (gdocs-diff--element-key '(:type specimen)) "specimen:stable"))
    (should (= (gdocs-diff--element-utf16-length '(:type specimen)) 0))
    (should (equal (gdocs-diff--runs-semantic-text
                    '((:text "  before  ") (:text "" :specimen t) (:text "  after  ")))
                   "before @specimen after"))
    (should (equal (gdocs-diff--run-formatting-key '(:text "" :specimen t)) "X"))
    (should (equal (gdocs-diff-semantic-run-requests
                    '((:text "😀") (:text "" :specimen t)) 7)
                   '(((specimenIndex . 9)))))
    (should (equal (gdocs-diff-generate-from-ops nil nil nil 17)
                   '(((specimenPlan . 17)))))))

(ert-deftest gdocs-diff-semantic-word-contract-accepts-an-unrelated-kind ()
  "Opaque inline tokens own retention and marker lifecycle, not the LCS engine."
  (let* ((remove '((deleteNamedRange . ((namedRangeId . "specimen-old")))))
         (retain (lambda (old new identical)
                   (not (and identical (/= (plist-get old :offset) (plist-get new :offset))))))
         (emit (lambda (index)
                 `((createNamedRange . ((name . "specimen")
                                        (range . ((startIndex . ,index) (endIndex . ,(+ index 5)))))))))
         (tokenizer
          (lambda (_runs &optional native)
            (if native
                (list '(:text "😀 " :offset 0 :length 3)
                      (list :text "Ready" :key 'specimen :offset 3 :length 5 :specimen t)
                      '(:text " Ready." :offset 8 :length 7))
              (list '(:text "😀 Ready " :offset 0 :length 9)
                    (list :text "Ready" :key 'specimen :offset 9 :length 5
                          :retain-p retain :insert-requests emit)
                    '(:text "." :offset 14 :length 1)))))
         (gdocs-diff-semantic-run-functions
          (list (lambda (run)
                  (when (plist-get run :specimen)
                    (list :text "@specimen" :key "X" :key-length 5
                          :content-change t :tokenizer tokenizer
                          :delete-requests (lambda (_runs) (list remove)))))))
         (old '((:type paragraph :style normal
                       :contents ((:text "😀 ") (:text "Ready" :specimen t) (:text " Ready.")))))
         (new '((:type paragraph :style normal
                       :contents ((:text "😀 Ready ") (:text "Ready" :specimen t) (:text ".")))))
         (requests (gdocs-diff-generate old new)))
    (should (equal (gdocs-diff--runs-formatting-key '((:text "Ready" :specimen t))) "X@5"))
    (should (equal (car requests) remove))
    (should (member '((createNamedRange . ((name . "specimen")
                                           (range . ((startIndex . 10) (endIndex . 15)))))) requests))
    (should-not (seq-some (lambda (request)
                            (or (alist-get 'insertText request) (alist-get 'deleteContentRange request)))
                          requests))
    (should (equal (car (gdocs-diff-generate old nil)) remove))))

(ert-deftest gdocs-diff-semantic-conflicting-word-policies-fail-closed ()
  "Independent tokenizers cannot silently override each other in one paragraph."
  (let ((gdocs-diff-semantic-run-functions
         (list (lambda (run)
                 (when-let* ((tokenizer (plist-get run :specimen-tokenizer)))
                   (list :tokenizer tokenizer))))))
    (should-error
     (gdocs-diff-semantic-paragraph
      '(:type paragraph :contents ((:text "one" :specimen-tokenizer ignore)
                                   (:text "two" :specimen-tokenizer identity))))
     :type 'user-error)))

(provide 'gdocs-diff-encapsulation-test)
;;; gdocs-diff-encapsulation-test.el ends here
