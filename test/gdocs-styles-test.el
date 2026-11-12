;;; gdocs-styles-test.el --- Presentation configuration contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration loads without transport and changes rendering, not semantics.

;;; Code:

(require 'ert)
(require 'cl-lib)

(ert-deftest gdocs-styles-loads-without-conversion-or-transport ()
  "Style configuration can be loaded on its own without the publishing client."
  (let ((root (file-name-directory (directory-file-name
                                    (file-name-directory (or load-file-name
                                                             (locate-library "gdocs-styles-test")))))))
    (with-temp-buffer
      (should
       (zerop
        (process-file
         (expand-file-name invocation-name invocation-directory) nil t nil
         "--batch" "-Q" "-L" root "--eval"
         "(progn (require 'gdocs-styles) (when (or (featurep 'gdocs-convert) (featurep 'gdocs-auth) (featurep 'gdocs-api)) (error \"Style configuration loaded transport/conversion\")))"))))))

(ert-deftest gdocs-styles-heading-overrides-change-requests-not-ir ()
  "Fonts, colours, and distances are supplied by configuration, not conversion."
  (require 'gdocs-styles)
  (require 'gdocs-convert)
  (let* ((source "*** Plain heading\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (before (copy-tree ir))
         (gdocs-style-definitions
          '((gdocs-heading-3 :parent heading-3
                             :text (:font-family "Example Font" :foreground-color "#123456")
                             :paragraph (:space-above 13 :space-below 9 :indent-start 17))))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (paragraph (cl-some
                     (lambda (request)
                       (let ((update (alist-get 'updateParagraphStyle request)))
                         (when (alist-get 'spaceAbove (alist-get 'paragraphStyle update)) update)))
                     requests))
         (text (cl-some
                (lambda (request)
                  (let ((update (alist-get 'updateTextStyle request)))
                    (when (alist-get 'weightedFontFamily (alist-get 'textStyle update)) update)))
                requests)))
    (should (equal ir before))
    (should (equal (gdocs-convert-ir-to-org ir) source))
    (should (equal (alist-get 'fontFamily
                              (alist-get 'weightedFontFamily (alist-get 'textStyle text)))
                   "Example Font"))
    (should (alist-get 'foregroundColor (alist-get 'textStyle text)))
    (should (= (alist-get 'magnitude (alist-get 'spaceAbove (alist-get 'paragraphStyle paragraph))) 13))
    (should (= (alist-get 'magnitude (alist-get 'spaceBelow (alist-get 'paragraphStyle paragraph))) 9))
    (should (= (alist-get 'magnitude (alist-get 'indentStart (alist-get 'paragraphStyle paragraph))) 17))))

(ert-deftest gdocs-styles-border-fallbacks-are-configurable ()
  "Fallback border colours, widths, and padding come from style configuration."
  (require 'gdocs-styles)
  (require 'gdocs-convert)
  (let* ((gdocs-style-default-border-color "#123456")
         (gdocs-style-default-border-width 3)
         (gdocs-style-default-border-padding 7)
         (border (gdocs-convert--paragraph-border-object nil nil nil)))
    (should (equal (alist-get 'color border) (gdocs-convert--hex-to-color "#123456")))
    (should (= (alist-get 'magnitude (alist-get 'width border)) 3))
    (should (= (alist-get 'magnitude (alist-get 'padding border)) 7))))

(provide 'gdocs-styles-test)
;;; gdocs-styles-test.el ends here
