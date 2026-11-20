;;; gdocs-inline-codec-test.el --- Inline conversion ownership -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared rich text conversion must not absorb semantic object policy.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gdocs-convert)
(require 'gdocs-diff)

(ert-deftest gdocs-inline-codec-rich-link-native-roundtrip ()
  "Linked emphasis, Unicode and source wrapping survive native import and edits."
  (require 'gdocs-convert-inline-codec)
  (let* ((source "A *[[https://example.org][😀 label]]* and /soft\nwrap/.\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (paragraph '((elements . [((textRun . ((content . "A "))))
                                   ((textRun . ((content . "😀 label")
                                                (textStyle . ((bold . t) (underline . t)
                                                              (link . ((url . "https://example.org"))))))))
                                   ((textRun . ((content . " and "))))
                                   ((textRun . ((content . "soft wrap") (textStyle . ((italic . t))))))
                                   ((textRun . ((content . ".\n"))))])))
         (native (gdocs-convert--docs-paragraph-to-ir paragraph nil nil 1 27))
         (returned (gdocs-convert-ir-to-org (list native)))
         (again (gdocs-convert-org-string-to-ir returned)))
    (should (equal (gdocs-diff--element-keys ir) (gdocs-diff--element-keys (list native))))
    (should (string-match-p (regexp-quote "*[[https://example.org][😀 label]]*") returned))
    (should-not (gdocs-diff-generate ir again))
    (should-not (gdocs-diff-generate again (gdocs-convert-org-string-to-ir returned)))
    (should (gdocs-convert-ir-to-docs-requests ir))
    (should (gdocs-diff-generate ir (gdocs-convert-org-string-to-ir "A *changed* link.\n")))))

(ert-deftest gdocs-inline-codec-has-physical-owners ()
  "Shared inline and semantic helpers have distinct physical owners."
  (dolist (entry '((gdocs-convert--objects-to-runs . "inline-codec")
                   (gdocs-convert--object-to-runs . "inline-codec")
                   (gdocs-convert--markup-to-runs . "inline-codec")
                   (gdocs-convert--link-to-runs . "inline-codec")
                   (gdocs-convert--docs-text-run-to-ir . "inline-codec")
                   (gdocs-convert--merge-adjacent-runs . "inline-codec")
                   (gdocs-convert--timestamp-to-runs . "dates")
                   (gdocs-convert--docs-date-element-to-run . "dates")
                   (gdocs-convert--make-insert-date-request . "dates")
                   (gdocs-convert-status-finalize-runs . "status")
                   (gdocs-convert--make-footnote-reference-requests . "footnotes")
                   (gdocs-convert--footnote-reference-to-runs . "footnotes")
                   (gdocs-convert--docs-footnote-reference-to-run . "footnotes")))
    (should (string-match-p (concat "gdocs-convert-" (cdr entry) "\\.elc?\\'")
                            (symbol-file (car entry) 'defun)))))

(ert-deftest gdocs-inline-codec-neutral-run-policy ()
  "An unrelated semantic kind can preserve atomic boundaries and whitespace."
  (require 'gdocs-convert-inline-codec)
  (let ((a (list :text " opaque " :fixture-object t))
        (b (list :text " opaque " :fixture-object t)))
    (cl-letf (((symbol-function 'gdocs-convert--inline-run-policy)
               (lambda (run)
                 (when (plist-get run :fixture-object)
                   '(:atomic t :identity fixture :preserve-whitespace t
                             :preserve-terminal t :formatted t)))))
      (should (= (length (gdocs-convert--merge-adjacent-runs (list a b))) 2))
      (should (equal (gdocs-convert--normalize-run-whitespace (list a)) (list a)))
      (should (equal (gdocs-convert--trim-trailing-newline-runs (list a)) (list a))))))

(defun gdocs-inline-codec-test--contains (tree symbol)
  "Return non-nil when TREE contains SYMBOL."
  (if (consp tree)
      (or (gdocs-inline-codec-test--contains (car tree) symbol)
          (gdocs-inline-codec-test--contains (cdr tree) symbol))
    (eq tree symbol)))

(ert-deftest gdocs-inline-codec-has-no-semantic-object-branches ()
  "Generic inline algorithms cannot retain status/date/reference classification."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "gdocs-convert-inline-codec.el"
                       (file-name-directory (locate-library "gdocs-convert"))))
    (goto-char (point-min))
    (let (forms)
      (condition-case nil (while t (push (read (current-buffer)) forms)) (end-of-file nil))
      (dolist (symbol '(:status :date-element :footnote-label footnoteReference dateElement dropdown
                                timestamp footnote-reference))
        (should-not (gdocs-inline-codec-test--contains forms symbol))))))

(ert-deftest gdocs-inline-codec-facade-only-composes-adapters ()
  "The facade must not keep semantic branches behind its adapter entry points."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "gdocs-convert.el"
                                            (file-name-directory (locate-library "gdocs-convert"))))
    (goto-char (point-min))
    (let (form)
      (condition-case nil
          (while t
            (setq form (read (current-buffer)))
            (when (and (eq (car-safe form) 'defun)
                       (memq (cadr form) '(gdocs-convert--typed-object-to-runs
                                           gdocs-convert--run-to-org
                                           gdocs-convert--inline-element-parts
                                           gdocs-convert--inline-run-policy
                                           gdocs-convert--make-inline-insert-requests
                                           gdocs-convert--run-model-length)))
              (dolist (symbol '(cond pcase :status :date-element :footnote-label
                                     footnoteReference dateElement dropdown org-element-type))
                (should-not (gdocs-inline-codec-test--contains form symbol)))))
        (end-of-file nil)))))

(ert-deftest gdocs-inline-codec-independent-with-generic-services ()
  "The inline codec loads without facade, style policy, features or transport."
  (let ((directory (file-name-directory (locate-library "gdocs-convert"))))
    (with-temp-buffer
      (let* ((program
              '(progn
                 (require 'gdocs-convert-inline-codec)
                 (dolist (feature '(gdocs-convert gdocs-styles gdocs-convert-status
                                                  gdocs-convert-footnotes gdocs-convert-dates
                                                  gdocs-api gdocs-auth))
                   (when (featurep feature) (error "Inline codec loaded %s" feature)))
                 (fset 'gdocs-convert--inline-link-for-org #'identity)
                 (fset 'gdocs-convert--inline-run-policy (lambda (_run) nil))
                 (let ((run (gdocs-convert-make-run "😀" :bold t :link "https://example.org")))
                   (unless (equal (gdocs-convert-inline-plain-run-to-org run)
                                  "*[[https://example.org][😀]]*")
                     (error "Standalone rich link rendering failed")))))
             (status (call-process (expand-file-name invocation-name invocation-directory) nil t nil
                                   "--batch" "-Q" "-L" directory "--eval" (prin1-to-string program))))
        (ert-info ((buffer-string)) (should (zerop status)))))))

(ert-deftest gdocs-inline-codec-unknown-native-objects-stop-instead-of-disappearing ()
  "Unsupported native union members cannot vanish from surrounding paragraph text."
  (dolist (kind '(person richLink equation futureInlineObject))
    (let* ((parts (vector '((textRun . ((content . "Before "))))
                          (list (cons kind '((displayText . "Synthetic object"))))
                          '((textRun . ((content . " after.\n"))))))
           (document (list (cons 'body (list (cons 'content
                                                   (vector (list (cons 'startIndex 1)
                                                                 (cons 'endIndex 30)
                                                                 (cons 'paragraph (list (cons 'elements parts)))))))))))
      (should-error (gdocs-convert-docs-json-to-ir document) :type 'user-error))))

(provide 'gdocs-inline-codec-test)
;;; gdocs-inline-codec-test.el ends here
