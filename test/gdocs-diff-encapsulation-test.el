;;; gdocs-diff-encapsulation-test.el --- Semantic boundary contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; The kernel knows generic IR/index/LCS mechanics.  Feature modules supply
;; semantic meaning through the same narrow protocol, including synthetic kinds.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'gdocs-diff)

(ert-deftest gdocs-diff-kernel-has-no-footnote-policy ()
  "Footnote identifiers and decisions belong outside the generic engine."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "gdocs-diff.el"
                       (file-name-directory (locate-library "gdocs-diff"))))
    (goto-char (point-min))
    (cl-labels ((inspect (form)
                  (cond
                   ((symbolp form)
                    (should-not (string-match-p "footnote" (symbol-name form))))
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

(provide 'gdocs-diff-encapsulation-test)
;;; gdocs-diff-encapsulation-test.el ends here
