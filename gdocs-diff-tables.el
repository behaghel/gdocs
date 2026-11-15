;;; gdocs-diff-tables.el --- Table diff identity and geometry -*- lexical-binding: t; -*-

;;; Commentary:
;; Table identity and read-side model length are table policy.  The LCS/index
;; kernel consumes the existing neutral element descriptor rather than table
;; branches.  Identity intentionally preserves the established text-only key.

;;; Code:

(require 'gdocs-convert-tables)
(defvar gdocs-diff-semantic-element-functions)

(defun gdocs-diff--table-key (element)
  "Return the established table ELEMENT key, concatenating cell text in order."
  (let (texts)
    (dolist (row (plist-get element :rows))
      (dolist (cell row) (push (gdocs-convert--runs-to-plain-text cell) texts)))
    (format "table:%s" (mapconcat #'identity (nreverse texts) "|"))))

(defun gdocs-diff--table-utf16-length (element)
  "Return table ELEMENT's native read-side length, excluding preserved paragraph."
  (gdocs-convert-table-body-length element))

(defun gdocs-diff-tables--element (element)
  "Describe table ELEMENT, or decline other kinds."
  (when (eq (plist-get element :type) 'table)
    (list :key (gdocs-diff--table-key element)
          :body-length (gdocs-diff--table-utf16-length element))))

(add-hook 'gdocs-diff-semantic-element-functions #'gdocs-diff-tables--element t)

(provide 'gdocs-diff-tables)
;;; gdocs-diff-tables.el ends here
