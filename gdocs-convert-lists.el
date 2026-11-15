;;; gdocs-convert-lists.el --- List paragraph conversion -*- lexical-binding: t; -*-

;;; Commentary:
;; List paragraph roles, native nesting/task state and Org list syntax are
;; owned here.  Plain paragraph decoding is shared; publication request grouping
;; and document orchestration remain separate responsibilities.

;;; Code:

(require 'cl-lib)
(require 's)
(require 'subr-x)
(require 'gdocs-convert-paragraph-codec)

(declare-function gdocs-convert--range-marker "gdocs-convert" (markers type start end))
(defun gdocs-convert--task-state-marker (markers start end)
  "Return task-state marker overlapping START..END, or nil."
  (gdocs-convert--range-marker markers 'task-state start end))

(defun gdocs-convert-list-paragraph-from-docs (paragraph lists-map markers runs start end)
  "Decode list PARAGRAPH with LISTS-MAP, MARKERS, RUNS and START..END, or nil."
  (when-let* ((bullet (alist-get 'bullet paragraph)))
    (let ((info (gdocs-convert--docs-bullet-to-list
                 bullet lists-map markers start end (alist-get 'paragraphStyle paragraph))))
      (gdocs-convert-paragraph-from-docs paragraph runs markers start end 'normal (list :list info)))))

(defun gdocs-convert-list-paragraph-to-org (element text)
  "Render list paragraph ELEMENT and TEXT, or nil when unhandled."
  (when-let* ((list-info (plist-get element :list)))
    (gdocs-convert--format-list-item list-info text)))

(defun gdocs-convert--format-list-item (list-info text)
  "Format a list item with LIST-INFO plist and TEXT content."
  (let* ((type (plist-get list-info :type))
         (level (plist-get list-info :level))
         (indent (make-string (* level 2) ?\s))
         (marker (gdocs-convert--list-marker type list-info))
         (cont-indent (make-string (+ (* level 2) (length marker)) ?\s))
         (indented-text (replace-regexp-in-string "\n" (concat "\n" cont-indent) text)))
    (concat indent marker indented-text)))

(defun gdocs-convert--list-marker (type list-info)
  "Return the org list marker string for TYPE and LIST-INFO."
  (pcase type
    ('bullet "- ")
    ('number (format "%d. " (or (plist-get list-info :counter) 1)))
    ('check (cond ((plist-get list-info :partial) "- [-] ")
                  ((plist-get list-info :checked) "- [X] ")
                  (t "- [ ] ")))))

(defun gdocs-convert--list-state-marker (markers start end)
  "Return list-state marker overlapping START..END, or nil."
  (gdocs-convert--range-marker markers 'list-state start end))

(defun gdocs-convert--list-state-data (data)
  "Return list plist decoded from list-state DATA."
  (when data
    (pcase-let* ((`(,type ,level) (s-split ":" data)))
      (list :type (intern type) :level (string-to-number level)))))

(defconst gdocs-convert--unchecked-checkbox-glyphs
  '("☐" "□" "❏" "❑" "◻" "⬜")
  "Google Docs glyph symbols observed for unchecked checklist items.")
(defconst gdocs-convert--checked-checkbox-glyphs '("☑" "✓" "✔")
  "Google Docs glyph symbols observed for checked checklist items.")
(defconst gdocs-convert--partial-checkbox-glyphs '("☒")
  "Google Docs glyph symbols observed for partial checklist items.")

(defun gdocs-convert--docs-check-state (list-props nesting-level)
  "Return checkbox state plist from LIST-PROPS at NESTING-LEVEL."
  (let* ((nesting (alist-get 'listProperties list-props))
         (levels (alist-get 'nestingLevels nesting))
         (level-info (when (and levels (> (length levels) nesting-level)) (aref levels nesting-level)))
         (glyph-symbol (when level-info (alist-get 'glyphSymbol level-info))))
    (cond ((member glyph-symbol gdocs-convert--checked-checkbox-glyphs) '(:checked t))
          ((member glyph-symbol gdocs-convert--partial-checkbox-glyphs) '(:partial t))
          (t '(:checked nil)))))

(defun gdocs-convert--docs-bullet-to-list
    (bullet lists-map &optional markers start end paragraph-style)
  "Decode native BULLET using LISTS-MAP, MARKERS, START, END and PARAGRAPH-STYLE."
  (let* ((list-id (alist-get 'listId bullet))
         (nesting-level (or (alist-get 'nestingLevel bullet) 0))
         (list-props (when list-id (alist-get (intern list-id) lists-map)))
         (list-type (gdocs-convert--docs-list-type list-props nesting-level))
         (level (gdocs-convert--docs-visual-nesting-level list-props nesting-level paragraph-style))
         (task-state (plist-get (gdocs-convert--task-state-marker markers start end) :data))
         (list-state (gdocs-convert--list-state-data
                      (plist-get (gdocs-convert--list-state-marker markers start end) :data)))
         (effective-type (or (plist-get list-state :type) (if task-state 'check list-type)))
         (effective-level (or (plist-get list-state :level) level)))
    (append (list :type effective-type :level effective-level)
            (when (eq effective-type 'check)
              (cond ((equal task-state "partial") '(:checked nil :partial t))
                    ((equal task-state "on") '(:checked t :partial nil))
                    ((equal task-state "off") '(:checked nil :partial nil))
                    (t (gdocs-convert--docs-check-state list-props nesting-level)))))))

(defun gdocs-convert--assign-list-counters (ir)
  "Assign sequential :counter values to numbered list items in IR."
  (let ((counter 0) (prev-level nil))
    (dolist (element ir)
      (let* ((list-info (plist-get element :list))
             (list-type (when list-info (plist-get list-info :type)))
             (level (when list-info (plist-get list-info :level))))
        (if (eq list-type 'number)
            (progn
              (if (eql level prev-level) (cl-incf counter) (setq counter 1))
              (plist-put list-info :counter counter)
              (setq prev-level level))
          (setq counter 0 prev-level nil))))))

(defconst gdocs-convert--list-indent-pt 36
  "Points of indentation per list nesting level in Google Docs.")

(defun gdocs-convert--dimension-magnitude (object field)
  "Return numeric magnitude for FIELD on Google Docs style OBJECT."
  (alist-get 'magnitude (alist-get field object)))

(defun gdocs-convert--nesting-level-from-indent (indent-first indent-start)
  "Return visual nesting level from INDENT-FIRST or INDENT-START."
  (cond
   (indent-first
    (let ((half-indent (/ (float gdocs-convert--list-indent-pt) 2)))
      (max 0 (round (/ (- indent-first half-indent) (float gdocs-convert--list-indent-pt))))))
   (indent-start (max 0 (1- (round (/ indent-start (float gdocs-convert--list-indent-pt))))))))

(defun gdocs-convert--docs-visual-nesting-level (list-props nesting-level &optional paragraph-style)
  "Compute visual NESTING-LEVEL from LIST-PROPS or fallback PARAGRAPH-STYLE."
  (let* ((nesting (alist-get 'listProperties list-props))
         (levels (alist-get 'nestingLevels nesting))
         (level-info (when (and levels (> (length levels) nesting-level)) (aref levels nesting-level)))
         (indent-first (or (gdocs-convert--dimension-magnitude level-info 'indentFirstLine)
                           (gdocs-convert--dimension-magnitude paragraph-style 'indentFirstLine)))
         (indent-start (or (gdocs-convert--dimension-magnitude level-info 'indentStart)
                           (gdocs-convert--dimension-magnitude paragraph-style 'indentStart))))
    (or (gdocs-convert--nesting-level-from-indent indent-first indent-start) nesting-level)))

(defun gdocs-convert--docs-list-type (list-props nesting-level)
  "Determine the IR list type from Google Docs LIST-PROPS at NESTING-LEVEL."
  (if (null list-props) 'bullet
    (let* ((nesting (alist-get 'listProperties list-props))
           (levels (alist-get 'nestingLevels nesting))
           (level-info (when (and levels (> (length levels) nesting-level)) (aref levels nesting-level)))
           (glyph-type (when level-info (alist-get 'glyphType level-info)))
           (glyph-symbol (when level-info (alist-get 'glyphSymbol level-info))))
      (cond ((gdocs-convert--checkbox-glyph-p glyph-type glyph-symbol) 'check)
            ((and glyph-type (member glyph-type '("DECIMAL" "ALPHA" "ROMAN" "ZERO_DECIMAL"))) 'number)
            (t 'bullet)))))

(defun gdocs-convert--checkbox-glyph-p (glyph-type glyph-symbol)
  "Return non-nil if GLYPH-TYPE or GLYPH-SYMBOL indicates a checkbox."
  (or (equal glyph-type "CHECKBOX")
      (and glyph-symbol (member glyph-symbol
                                (append gdocs-convert--unchecked-checkbox-glyphs
                                        gdocs-convert--checked-checkbox-glyphs
                                        gdocs-convert--partial-checkbox-glyphs)))))

(provide 'gdocs-convert-lists)
;;; gdocs-convert-lists.el ends here
