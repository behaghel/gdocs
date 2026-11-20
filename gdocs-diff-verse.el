;;; gdocs-diff-verse.el --- Verse identity and physical geometry -*- lexical-binding: t; -*-

;;; Commentary:
;; Generic diff consumes opaque identity and UTF-16 geometry descriptors.

;;; Code:

(require 'gdocs-convert-verse)
(defvar gdocs-diff-semantic-element-functions)
(declare-function gdocs-diff--runs-formatting-key "gdocs-diff" (runs))

(defun gdocs-diff-verse--element (element)
  "Describe verse ELEMENT without teaching generic diff verse policy."
  (when (gdocs-convert-verse-element-p element)
    (let ((lines (or (plist-get element :lines) (list nil))))
      (list :key (format "verse:%S:%S" (plist-get element :style-revision)
                         (mapcar (lambda (runs)
                                   (list (gdocs-convert--runs-to-plain-text runs)
                                         (gdocs-diff--runs-formatting-key runs))) (plist-get element :lines)))
            :body-length (1+ (apply #'+ (mapcar (lambda (runs)
                                                  (1+ (gdocs-convert--string-to-utf16-length
                                                       (gdocs-convert--runs-to-plain-text runs)))) lines)))))))

(add-hook 'gdocs-diff-semantic-element-functions #'gdocs-diff-verse--element t)

(provide 'gdocs-diff-verse)
;;; gdocs-diff-verse.el ends here
