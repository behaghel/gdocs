;;; gdocs-diff-centers.el --- Centered block identity and geometry -*- lexical-binding: t; -*-

;;; Commentary:
;; Generic diff consumes feature-owned content identity and UTF-16 descriptors.

;;; Code:

(require 'gdocs-convert-centers)
(defvar gdocs-diff-semantic-element-functions)
(declare-function gdocs-diff--runs-formatting-key "gdocs-diff" (runs))

(defun gdocs-diff-centers--element (element)
  "Describe centered ELEMENT identity and physical body length."
  (when (gdocs-convert-center-element-p element)
    (list :key (format "center:%S:%S" (plist-get element :style-revision)
                       (mapcar (lambda (runs) (list (gdocs-convert--runs-to-plain-text runs)
                                                    (gdocs-diff--runs-formatting-key runs))) (plist-get element :paragraphs)))
          :body-length (1+ (apply #'+ (mapcar (lambda (runs)
                                                (1+ (gdocs-convert--string-to-utf16-length (gdocs-convert--runs-to-plain-text runs))))
                                              (or (plist-get element :paragraphs) (list nil))))))))

(add-hook 'gdocs-diff-semantic-element-functions #'gdocs-diff-centers--element t)

(provide 'gdocs-diff-centers)
;;; gdocs-diff-centers.el ends here
