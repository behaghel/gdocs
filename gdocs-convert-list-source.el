;;; gdocs-convert-list-source.el --- Org list recognition -*- lexical-binding: t; -*-

;;; Commentary:
;; Source list syntax, item traversal and structural indentation belong here.
;; Document IDs and inline conversion remain explicit document services.

;;; Code:

(require 'org-element)
(declare-function gdocs-convert-list-item-positioned "gdocs-convert-lists" (item fallback depth))

(defun gdocs-convert-list-source-positioned (plain-list)
  "Return positioned top-level PLAIN-LIST elements, or nil for nested lists."
  (when (gdocs-convert--top-level-list-p plain-list)
    (gdocs-convert--plain-list-positioned plain-list 0)))

(defun gdocs-convert--plain-list-positioned (plain-list depth)
  "Convert PLAIN-LIST to positioned IR entries at nesting DEPTH.
Returns a list of (:ir ELEMENT :begin POS) plists."
  (let ((list-type (gdocs-convert--org-list-type plain-list)) result)
    (dolist (item (org-element-contents plain-list))
      (when (eq (org-element-type item) 'item)
        (setq result (nconc result (gdocs-convert--item-positioned item list-type depth)))))
    result))

(defun gdocs-convert--item-list-type (item fallback-type)
  "Return list type for ITEM, falling back to FALLBACK-TYPE.
A checkbox on one item does not turn sibling plain bullets into tasks."
  (let ((bullet (or (org-element-property :bullet item) "")))
    (cond ((org-element-property :checkbox item) 'check)
          ((string-match-p "\\`[0-9]+[.)][[:space:]]+" bullet) 'number)
          ((string-match-p "\\`[A-Za-z]+[.)][[:space:]]+" bullet) 'number)
          ((eq fallback-type 'check) 'bullet)
          (t fallback-type))))

(defun gdocs-convert--item-positioned (item list-type depth)
  "Convert ITEM with LIST-TYPE at DEPTH to positioned paragraph entries."
  (gdocs-convert-list-item-positioned item list-type depth))

(defun gdocs-convert--top-level-list-p (plain-list)
  "Return non-nil if PLAIN-LIST is a direct document or section child."
  (and (memq (org-element-type (org-element-parent plain-list)) '(section org-data)) t))

(defun gdocs-convert--org-list-type (plain-list)
  "Determine the IR list type symbol for PLAIN-LIST."
  (pcase (org-element-property :type plain-list)
    ('ordered 'number)
    ('descriptive 'bullet)
    (_ (if (gdocs-convert--list-has-checkboxes-p plain-list) 'check 'bullet))))

(defun gdocs-convert--list-has-checkboxes-p (plain-list)
  "Return non-nil if any item in PLAIN-LIST has a checkbox."
  (org-element-map plain-list 'item (lambda (item) (org-element-property :checkbox item)) nil t))

(defun gdocs-convert--strip-continuation-indent (runs)
  "Strip structural continuation indentation from source item RUNS."
  (mapcar (lambda (run)
            (let ((text (plist-get run :text)))
              (if (string-match-p "\n" text)
                  (plist-put (copy-sequence run) :text (replace-regexp-in-string "\n[ \t]+" "\n" text))
                run))) runs))

(provide 'gdocs-convert-list-source)
;;; gdocs-convert-list-source.el ends here
