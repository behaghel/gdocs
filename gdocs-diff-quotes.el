;;; gdocs-diff-quotes.el --- Quote identity and body geometry -*- lexical-binding: t; -*-

;;; Commentary:
;; Quote identity includes authored runs and style revision.  Its native body
;; includes generated separator chrome.  Generic diff consumes descriptors.

;;; Code:

(require 'gdocs-convert-text)
(defvar gdocs-diff-semantic-element-functions)
(declare-function gdocs-diff--runs-formatting-key "gdocs-diff" (runs))

(defun gdocs-diff--quote-block-key (element)
  "Compute a content key for quote-block ELEMENT."
  (format "quote-block:%s:%s"
          (or (plist-get element :style-revision) "")
          (mapconcat (lambda (runs)
                       (format "%s:%s" (gdocs-diff--runs-formatting-key runs)
                               (gdocs-convert--runs-to-plain-text runs)))
                     (plist-get element :paragraphs) "\n\n")))

(defun gdocs-diff--quote-block-utf16-length (element)
  "Compute UTF-16 length of quote-block ELEMENT, including separator chrome."
  (1+ (apply #'+
             (mapcar (lambda (runs)
                       (1+ (gdocs-convert--string-to-utf16-length
                            (gdocs-convert--runs-to-plain-text runs))))
                     (or (plist-get element :paragraphs)
                         (list (list (gdocs-convert--make-plain-run ""))))))))

(defun gdocs-diff-quotes--element (element)
  "Describe quote ELEMENT, or decline other kinds."
  (when (eq (plist-get element :type) 'quote-block)
    (list :key (gdocs-diff--quote-block-key element)
          :body-length (gdocs-diff--quote-block-utf16-length element))))

(add-hook 'gdocs-diff-semantic-element-functions #'gdocs-diff-quotes--element t)

(provide 'gdocs-diff-quotes)
;;; gdocs-diff-quotes.el ends here
