;;; gdocs-paragraph-codec-test.el --- Bidirectional paragraph ownership -*- lexical-binding: t; -*-

;;; Commentary:
;; Plain conversion composes generic inline services and feature-owned adapters.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gdocs-convert)
(require 'gdocs-diff)

(ert-deftest gdocs-paragraph-codec-plain-roundtrip-and-native-import ()
  "Org and native Docs plain paragraphs use one bidirectional codec."
  (require 'gdocs-convert-paragraph-codec)
  (let* ((org "Plain 😀 and *bold*\nsoft wrap.\n")
         (ir (gdocs-convert-org-string-to-ir org))
         (json '((body . ((content . [((startIndex . 1) (endIndex . 30)
                                       (paragraph . ((elements . [((textRun . ((content . "Plain 😀 and "))))
                                                                  ((textRun . ((content . "bold") (textStyle . ((bold . t))))))
                                                                  ((textRun . ((content . " soft wrap.\n"))))])
                                                     (paragraphStyle . ((namedStyleType . "NORMAL_TEXT"))))))])))))
         (remote (gdocs-convert-docs-json-to-ir json))
         (returned (gdocs-convert-ir-to-org remote))
         (again (gdocs-convert-org-string-to-ir returned)))
    (should (equal (gdocs-diff--element-keys ir) (gdocs-diff--element-keys remote)))
    (should-not (gdocs-diff-generate ir again))
    (should-not (gdocs-diff-generate again (gdocs-convert-org-string-to-ir returned)))
    (should (gdocs-convert-ir-to-docs-requests ir))))

(ert-deftest gdocs-paragraph-codec-rendering-accepts-an-unrelated-feature ()
  "Feature rendering composes without adding a semantic branch to the codec."
  (require 'gdocs-convert-paragraph-codec)
  (let ((element (gdocs-convert-make-paragraph (list (gdocs-convert-make-run "Body")))))
    (should (equal (gdocs-convert-paragraph-to-org element) "Body"))
    (should (equal (gdocs-convert-paragraph-to-org
                    element (list (lambda (_element text) (concat "SPECIMEN[" text "]"))))
                   "SPECIMEN[Body]"))))

(ert-deftest gdocs-paragraph-codec-definitions-own-plain-conversion ()
  "Plain source-child and native alignment helpers belong to the codec."
  (dolist (function '(gdocs-convert--paragraph-child-to-ir gdocs-convert--docs-alignment-to-ir))
    (should (string-match-p "gdocs-convert-paragraph-codec\\.elc?\\'"
                            (symbol-file function 'defun)))))

(defun gdocs-paragraph-codec-test--contains (tree symbol)
  "Return non-nil when TREE contains SYMBOL."
  (if (consp tree)
      (or (gdocs-paragraph-codec-test--contains (car tree) symbol)
          (gdocs-paragraph-codec-test--contains (cdr tree) symbol))
    (eq tree symbol)))

(ert-deftest gdocs-paragraph-codec-facade-only-composes-feature-adapters ()
  "Paragraph orchestration must not retain semantic recognition or formatting."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "gdocs-convert.el"
                                            (file-name-directory (locate-library "gdocs-convert"))))
    (goto-char (point-min))
    (let (form)
      (condition-case nil
          (while t
            (setq form (read (current-buffer)))
            (when (and (eq (car-safe form) 'defun)
                       (memq (cadr form) '(gdocs-convert--paragraph-to-ir
                                           gdocs-convert--docs-paragraph-to-ir
                                           gdocs-convert--ir-paragraph-to-org)))
              (dolist (symbol '(cond pcase org-element-type
                                     gdocs-convert--heading-style-p
                                     gdocs-convert--extract-marker-field
                                     gdocs-convert--normalize-heading-bold
                                     gdocs-convert--multiline-code-paragraph-p))
                (should-not (gdocs-paragraph-codec-test--contains form symbol)))))
        (end-of-file nil)))))

(ert-deftest gdocs-paragraph-codec-feature-policies-have-physical-owners ()
  "Feature helpers are not left behind as policy branches in the facade."
  (dolist (entry '((gdocs-convert--standalone-image-paragraph-to-ir . "images")
                   (gdocs-convert--multiline-code-paragraph-p . "org-blocks")
                   (gdocs-convert--format-src-block . "code-blocks")
                   (gdocs-convert--docs-bullet-to-list . "lists")
                   (gdocs-convert--format-list-item . "lists")
                   (gdocs-convert--docs-style-to-ir . "headings")))
    (should (string-match-p (concat "gdocs-convert-" (cdr entry) "\\.elc?\\'")
                            (symbol-file (car entry) 'defun)))))

(ert-deftest gdocs-paragraph-codec-feature-precedence-and-single-decoding ()
  "Native list roles outrank heading styles without decoding inline data twice."
  (let ((original (symbol-function 'gdocs-convert--docs-elements-to-runs)) (calls 0))
    (cl-letf (((symbol-function 'gdocs-convert--docs-elements-to-runs)
               (lambda (&rest args) (setq calls (1+ calls)) (apply original args))))
      (let* ((paragraph '((paragraphStyle . ((namedStyleType . "HEADING_2")))
                          (bullet . ((listId . "fixture-list")))
                          (elements . [((textRun . ((content . "Item\n")
                                                    (textStyle . ((bold . t))))))])))
             (ir (gdocs-convert--docs-paragraph-to-ir paragraph nil nil 1 6)))
        (should (eq (plist-get ir :style) 'normal))
        (should (eq (plist-get (plist-get ir :list) :type) 'bullet))
        (should (plist-get (car (plist-get ir :contents)) :bold))
        (should (equal (gdocs-convert--ir-paragraph-to-org ir) "- *Item*"))
        (should (= calls 1))))))

(ert-deftest gdocs-paragraph-codec-images-precede-inline-decoding ()
  "An image paragraph is claimed before inline link conversion can run."
  (with-temp-buffer
    (let ((org-inhibit-startup t)) (delay-mode-hooks (org-mode)))
    (insert "[[file:fixture-image.png]]\n")
    (let ((paragraph (org-element-map (org-element-parse-buffer) 'paragraph #'identity nil t)))
      (cl-letf (((symbol-function 'gdocs-convert--objects-to-runs)
                 (lambda (&rest _) (ert-fail "Image was passed to generic inline decoding"))))
        (let ((ir (car (gdocs-convert--paragraph-to-ir paragraph))))
          (should (eq (plist-get ir :type) 'image))
          (should (equal (plist-get ir :path) "fixture-image.png")))))))

(ert-deftest gdocs-paragraph-codec-has-no-feature-recognition ()
  "The shared codec cannot absorb moved semantic branches in later changes."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "gdocs-convert-paragraph-codec.el"
                       (file-name-directory (locate-library "gdocs-convert"))))
    (goto-char (point-min))
    (let (forms)
      (condition-case nil (while t (push (read (current-buffer)) forms)) (end-of-file nil))
      (dolist (symbol '(org-element-type gdocs-convert--heading-style-p
                                         gdocs-convert--extract-marker-field gdocs-convert--docs-bullet-to-list
                                         :list :footnote-label :status :code))
        (should-not (gdocs-paragraph-codec-test--contains forms symbol))))))

(ert-deftest gdocs-paragraph-codec-loads-without-facade-or-feature-policy ()
  "The codec loads independently and accepts explicit generic document services."
  (let ((directory (file-name-directory (locate-library "gdocs-convert"))))
    (with-temp-buffer
      (let* ((program
              '(progn
                 (require 'gdocs-convert-paragraph-codec)
                 (dolist (feature '(gdocs-convert gdocs-styles gdocs-convert-images
                                                  gdocs-convert-lists gdocs-convert-headings
                                                  gdocs-convert-org-blocks gdocs-convert-status
                                                  gdocs-convert-footnotes gdocs-api gdocs-auth))
                   (when (featurep feature) (error "Codec loaded policy %s" feature)))
                 (fset 'gdocs-convert--next-id (lambda () "fixture-id"))
                 (let ((ir (gdocs-convert-paragraph-from-runs
                            (list (gdocs-convert-make-run "Body")))))
                   (unless (and (equal (plist-get ir :id) "fixture-id")
                                (eq (plist-get ir :style) 'normal))
                     (error "Plain codec failed with generic ID service")))))
             (status (call-process (expand-file-name invocation-name invocation-directory)
                                   nil t nil "--batch" "-Q" "-L" directory
                                   "--eval" (prin1-to-string program))))
        (ert-info ((buffer-string)) (should (zerop status)))))))

(ert-deftest gdocs-paragraph-codec-native-footnotes-compose-run-decoding ()
  "Native footnotes reuse paragraph decoding without moving footnote policy."
  (let ((decoder (symbol-function 'gdocs-convert-paragraph-docs-runs)) (calls 0))
    (cl-letf (((symbol-function 'gdocs-convert-paragraph-docs-runs)
               (lambda (&rest args) (setq calls (1+ calls)) (apply decoder args))))
      (let* ((element '((textRun . ((content . " Note\n")))))
             (paragraph (list (cons 'elements (list element))))
             (content (list (list (cons 'paragraph paragraph))))
             (runs (gdocs-convert--docs-footnote-content-to-runs content)))
        (should (equal (gdocs-convert--runs-to-plain-text runs) "Note"))
        (should (= calls 1))))))

(provide 'gdocs-paragraph-codec-test)
;;; gdocs-paragraph-codec-test.el ends here
