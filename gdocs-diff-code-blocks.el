;;; gdocs-diff-code-blocks.el --- Code/example diff semantics -*- lexical-binding: t; -*-

;;; Commentary:
;; Source language, style revision, literal lines and newline addressing belong
;; to the code/example feature, not the generic LCS and index algorithms.

;;; Code:

(require 'gdocs-convert-text)
(defvar gdocs-diff-semantic-element-functions)

(defun gdocs-diff--source-block-key (element)
  "Compute a content key for a source-block ELEMENT."
  (format "source-block:%s:%s:%s" (or (plist-get element :language) "")
          (or (plist-get element :style-revision) "")
          (mapconcat #'identity (plist-get element :lines) "\n")))

(defun gdocs-diff--example-block-key (element)
  "Compute a content key for example-block ELEMENT."
  (format "example-block:%s" (mapconcat #'identity (plist-get element :lines) "\n")))

(defun gdocs-diff-code-blocks--body-length (element)
  "Return literal UTF-16 line lengths plus terminal newlines for ELEMENT."
  (apply #'+ (mapcar (lambda (line) (1+ (gdocs-convert--string-to-utf16-length line)))
                     (or (plist-get element :lines) '("")))))

(defun gdocs-diff--source-block-utf16-length (element)
  "Compute UTF-16 length of source-block ELEMENT."
  (gdocs-diff-code-blocks--body-length element))

(defun gdocs-diff--example-block-utf16-length (element)
  "Compute UTF-16 length of example-block ELEMENT."
  (gdocs-diff-code-blocks--body-length element))

(defun gdocs-diff-code-blocks--element (element)
  "Describe code/example ELEMENT, or decline other kinds."
  (pcase (plist-get element :type)
    ('source-block (list :key (gdocs-diff--source-block-key element)
                         :body-length (gdocs-diff--source-block-utf16-length element)))
    ('example-block (list :key (gdocs-diff--example-block-key element)
                          :body-length (gdocs-diff--example-block-utf16-length element)))))

(add-hook 'gdocs-diff-semantic-element-functions #'gdocs-diff-code-blocks--element t)

(provide 'gdocs-diff-code-blocks)
;;; gdocs-diff-code-blocks.el ends here
