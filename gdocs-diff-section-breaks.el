;;; gdocs-diff-section-breaks.el --- Section break identity and geometry -*- lexical-binding: t; -*-

;;; Commentary:
;; A published break uses one marked newline plus an outside separator.
;; Native horizontal-rule paragraphs use an object plus their terminal newline.

;;; Code:

(require 'gdocs-convert-section-breaks)
(defvar gdocs-diff-semantic-element-functions)

(defun gdocs-diff-section-breaks--element (element)
  "Describe section-break ELEMENT identity and UTF-16 body geometry."
  (when (gdocs-convert-section-break-element-p element)
    (list :key (format "section-break:%S" (plist-get element :style-revision))
          :body-length (if (and (integerp (plist-get element :doc-start)) (integerp (plist-get element :doc-end)))
                           (- (plist-get element :doc-end) (plist-get element :doc-start))
                         2))))

(add-hook 'gdocs-diff-semantic-element-functions #'gdocs-diff-section-breaks--element t)

(provide 'gdocs-diff-section-breaks)
;;; gdocs-diff-section-breaks.el ends here
