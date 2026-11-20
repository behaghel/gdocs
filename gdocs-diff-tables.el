;;; gdocs-diff-tables.el --- Table diff identity and geometry -*- lexical-binding: t; -*-

;;; Commentary:
;; Table identity and read-side model length are table policy.  The LCS/index
;; kernel consumes the existing neutral element descriptor rather than table
;; branches.  Identity preserves row/cell boundaries and authored formatting.

;;; Code:

(require 'gdocs-convert-tables)
(defvar gdocs-diff-semantic-element-functions)
(declare-function gdocs-diff--runs-formatting-key "gdocs-diff" (runs))

(defun gdocs-diff--table-key (element)
  "Return a semantic table ELEMENT key including shape and rich cell content."
  (format "table:%S"
          (mapcar (lambda (row)
                    (mapcar (lambda (cell)
                              (list (gdocs-convert--runs-to-plain-text cell)
                                    (gdocs-diff--runs-formatting-key cell)))
                            row))
                  (plist-get element :rows))))

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
