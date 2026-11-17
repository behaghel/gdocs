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
(require 'gdocs-convert-tasks)
(require 'gdocs-convert-list-source)
(require 'gdocs-convert-list-publish)

(declare-function gdocs-convert--range-marker "gdocs-convert" (markers type start end))
(declare-function gdocs-convert--next-id "gdocs-convert" ())
(declare-function gdocs-convert--item-list-type "gdocs-convert" (item fallback-type))
(declare-function gdocs-convert--objects-to-runs "gdocs-convert" (objects &optional parent-style))
(declare-function gdocs-convert--strip-continuation-indent "gdocs-convert" (runs))
(declare-function gdocs-convert--trim-runs "gdocs-convert" (runs))
(declare-function gdocs-convert--plain-list-positioned "gdocs-convert" (plain-list depth))
(declare-function gdocs-convert--run-model-length "gdocs-convert" (run))
(declare-function gdocs-convert--list-prefixes-before "gdocs-convert" (range element-ranges))
(defun gdocs-convert--task-state-marker (markers start end)
  "Return task-state marker overlapping START..END, or nil."
  (gdocs-convert--range-marker markers 'task-state start end))

(defun gdocs-convert-list-paragraph-from-docs (paragraph lists-map markers runs start end)
  "Decode list PARAGRAPH with LISTS-MAP, MARKERS, RUNS and START..END, or nil."
  (or (gdocs-convert-list-continuation-from-docs paragraph runs markers start end)
      (when-let* ((bullet (alist-get 'bullet paragraph)))
        (gdocs-convert-task-validate-native-list paragraph lists-map markers start end)
        (let ((info (gdocs-convert--docs-bullet-to-list
                     bullet lists-map markers start end (alist-get 'paragraphStyle paragraph))))
          (gdocs-convert-paragraph-from-docs paragraph runs markers start end 'normal (list :list info))))))

(defun gdocs-convert-list-paragraph-to-org (element text)
  "Render list paragraph ELEMENT and TEXT, or nil when unhandled."
  (if-let* ((info (plist-get element :list-continuation)))
      (let ((indent (make-string (* 2 (1+ (plist-get info :level))) ?\s)))
        (concat indent (replace-regexp-in-string "\n" (concat "\n" indent) text)))
    (when-let* ((list-info (plist-get element :list)))
      (gdocs-convert--format-list-item list-info text))))

(defun gdocs-convert-list-paragraph-info (element)
  "Return ELEMENT's native, managed-task or continuation list membership."
  (or (plist-get element :list) (gdocs-convert-list-continuation-p element)
      (gdocs-convert-task-list-info element)))

(defun gdocs-convert-list-compact-neighbor-p (previous element)
  "Return non-nil when list ELEMENT remains compact after PREVIOUS."
  (let ((info (gdocs-convert-list-paragraph-info element))
        (before (gdocs-convert-list-paragraph-info previous)))
    (and info (or (gdocs-convert-list-continuation-p element)
                  (and before (eq (plist-get before :type) (plist-get info :type)))))))

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
         (native-levels (alist-get 'nestingLevels (alist-get 'listProperties list-props)))
         (native-known (and native-levels (> (length native-levels) nesting-level)))
         (native-entry (and native-known (elt native-levels nesting-level)))
         (native-depth-known (or (> nesting-level 0) (alist-get 'indentFirstLine native-entry)
                                 (alist-get 'indentStart native-entry)))
         (list-type (gdocs-convert--docs-list-type list-props nesting-level))
         (level (gdocs-convert--docs-visual-nesting-level list-props nesting-level paragraph-style))
         (task-state (plist-get (gdocs-convert--task-state-marker markers start end) :data))
         (list-state (gdocs-convert--list-state-data
                      (plist-get (gdocs-convert--list-state-marker markers start end) :data)))
         (effective-type (cond (task-state 'check) (native-known list-type)
                               (t (or (plist-get list-state :type) list-type))))
         ;; A native group's level zero can itself be nested in a mixed-type
         ;; list.  Its definition supplies that base offset; paragraph-only
         ;; presentation overrides and stale snapshots do not redefine depth.
         (effective-level (if native-depth-known
                              (gdocs-convert--docs-visual-nesting-level list-props nesting-level)
                            (or (plist-get list-state :level) level))))
    (append (list :type effective-type :level effective-level)
            (when (eq effective-type 'check)
              (cond ((equal task-state "partial") '(:checked nil :partial t))
                    ((equal task-state "on") '(:checked t :partial nil))
                    ((equal task-state "off") '(:checked nil :partial nil))
                    (t (gdocs-convert--docs-check-state list-props nesting-level)))))))

(defun gdocs-convert--assign-list-counters (ir)
  "Assign sequential counters per nesting level in IR, retaining parent state."
  (let (counters owners)
    (dolist (element ir)
      (let* ((continuation (plist-get element :list-continuation))
             (info (or (plist-get element :list) continuation))
             (type (plist-get info :type)) (level (plist-get info :level)))
        (if (null info) (setq counters nil owners nil)
          (setq counters (seq-filter (lambda (entry) (<= (car entry) level)) counters)
                owners (seq-filter (lambda (entry) (<= (car entry) level)) owners))
          (when (and continuation (not (eq (cdr (assq level owners)) type)))
            (user-error "List continuation has no preceding item of the same type and level"))
          (unless continuation (setf (alist-get level owners) type))
          (if (eq type 'number)
              (let ((counter (+ (or (alist-get level counters) 0) (if continuation 0 1))))
                (setf (alist-get level counters) counter)
                (plist-put info :counter counter))
            (setq counters (assq-delete-all level counters))))))))

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

(defun gdocs-convert-list-item-positioned (item fallback depth)
  "Convert ITEM at DEPTH, retaining each direct paragraph and child list.
FALLBACK is the containing list's type.  Continuations have no new bullet."
  (let* ((type (gdocs-convert--item-list-type item fallback))
         (state (org-element-property :checkbox item))
         (tag (org-element-property :tag item))
         (first t) result)
    (cl-labels ((emit (paragraph)
                  (let* ((info (list :type type :level depth))
                         (runs (when paragraph
                                 (gdocs-convert--trim-runs
                                  (gdocs-convert--strip-continuation-indent
                                   (gdocs-convert--objects-to-runs (org-element-contents paragraph))))))
                         (body (if (and first tag)
                                   (append (gdocs-convert--trim-runs
                                            (gdocs-convert--objects-to-runs (if (stringp tag) (list tag) tag)))
                                           (list (gdocs-convert--make-plain-run " :: ")) runs)
                                 runs))
                         (element (list :type 'paragraph :style 'normal
                                        :contents (or body (list (gdocs-convert--make-plain-run "")))
                                        :id (gdocs-convert--next-id))))
                    (if first
                        (progn
                          (when (eq type 'check)
                            (setq info (append info (list :checked (eq state 'on) :partial (eq state 'trans)))))
                          (plist-put element :list info)
                          (plist-put element :gdocs-marker
                                     (append (list (list :type 'list-state :data (format "%s:%d" type depth)))
                                             (when state (list (list :type 'task-state
                                                                     :data (pcase state ('on "on") ('trans "partial") (_ "off"))))))))
                      (plist-put element :list-continuation info)
                      (plist-put element :gdocs-marker
                                 (list (list :type 'list-continuation :data (format "v1:%s:%d" type depth)))))
                    (push (list :ir element :begin (if first (org-element-property :begin item)
                                                     (org-element-property :begin paragraph))) result)
                    (setq first nil))))
               (dolist (child (org-element-contents item))
                 (pcase (org-element-type child)
                   ('paragraph (emit child))
                   ('plain-list
                    (when first (emit nil))
                    (dolist (entry (gdocs-convert--plain-list-positioned child (1+ depth))) (push entry result)))
                   (_ (user-error "Unsupported %s inside a list item; move it outside the list before publishing"
                                  (org-element-type child)))))
               (when first (emit nil)))
    (nreverse result)))

(defun gdocs-convert-list-continuation-p (element)
  "Return continuation information on ELEMENT, or nil."
  (plist-get element :list-continuation))

(defun gdocs-convert-list-continuation-key (element)
  "Return continuation semantic identity for ELEMENT, or nil."
  (when-let* ((info (gdocs-convert-list-continuation-p element)))
    (format "continuation:%s:%d" (plist-get info :type) (plist-get info :level))))

(defun gdocs-convert-list-project-ir (ir)
  "Project IR's continuation prefixes and contextual list-spacing roles."
  (let (previous projected)
    (dolist (element ir (nreverse projected))
      (let ((paragraph (gdocs-convert-list-project-paragraph element)))
        (when (and (gdocs-convert-list-paragraph-info paragraph)
                   (not (plist-get paragraph :list-spacing-context)))
          (setq paragraph (copy-sequence paragraph))
          (plist-put paragraph :gdocs-list-spacing-style
                     (if (gdocs-convert-list-compact-neighbor-p previous paragraph)
                         'gdocs-list-item 'gdocs-list-first)))
        (push paragraph projected)
        (setq previous paragraph)))))

(defun gdocs-convert-list-project-paragraph (element)
  "Project ELEMENT's continuation annotation without changing body text."
  (if-let* ((info (gdocs-convert-list-continuation-p element)))
      (let* ((copy (copy-sequence element))
             (prefix (gdocs-convert--make-plain-run ""))
             (length (1+ (apply #'+ (mapcar #'gdocs-convert--run-model-length (plist-get element :contents))))))
        (plist-put copy :gdocs-marker
                   (seq-remove (lambda (marker) (eq (plist-get marker :type) 'list-continuation))
                               (plist-get element :gdocs-marker)))
        (unless (plist-get (car (plist-get element :contents)) :list-continuation-prefix)
          (plist-put prefix :list-continuation-prefix
                     (list :type (plist-get info :type) :level (plist-get info :level) :length length
                           :indent (or (plist-get element :list-continuation-indent-pt)
                                       (* 36 (1+ (plist-get info :level))))))
          (plist-put copy :contents (cons prefix (plist-get element :contents))))
        copy)
    element))

(defun gdocs-convert-list-continuation-run-policy (run)
  "Return zero-width annotation policy for continuation prefix RUN."
  (when (plist-get run :list-continuation-prefix)
    '(:atomic t :model-length 0 :preserve-whitespace t :preserve-terminal t :formatted t)))

(defun gdocs-convert-list-continuation-run-requests (run index &optional segment-id)
  "Plan continuation RUN's annotation and indentation at INDEX."
  (when-let* ((info (plist-get run :list-continuation-prefix)))
    (when segment-id (user-error "List continuations require body paragraphs, not document segments"))
    (list `((createNamedRange
             . ((name . ,(format "gdocs-org-marker:list-continuation:continuation-%d:v1:%s:%d"
                                 index (plist-get info :type) (plist-get info :level)))
                (range . ((startIndex . ,index) (endIndex . ,(+ index (plist-get info :length))))))))
          `((deleteParagraphBullets . ((range . ((startIndex . ,index) (endIndex . ,(1+ index)))))))
          `((updateParagraphStyle
             . ((paragraphStyle . ((indentStart . ((magnitude . ,(plist-get info :indent)) (unit . "PT")))
                                   (indentFirstLine . ((magnitude . ,(plist-get info :indent)) (unit . "PT")))))
                (range . ((startIndex . ,index) (endIndex . ,(1+ index))))
                (fields . "indentStart,indentFirstLine")))))))

(defun gdocs-convert-list-continuation-native-group (element)
  "Return ELEMENT's native continuation group, excluding managed glyph tasks."
  (when-let* ((info (gdocs-convert-list-continuation-p element)))
    (unless (eq (plist-get info :type) 'check) info)))

(defun gdocs-convert-list-continuation-post-bullet-indents (ranges)
  "Remove temporary continuation bullets and restore indentation in RANGES."
  (cl-loop for range in ranges
           for run = (car (plist-get (plist-get range :element) :contents))
           when (plist-get run :list-continuation-prefix)
           append (cdr (gdocs-convert-list-continuation-run-requests
                        run (- (plist-get range :start)
                               (gdocs-convert--list-prefixes-before range ranges))))))

(defun gdocs-convert-list-continuation-from-docs (paragraph runs markers start end)
  "Restore continuation PARAGRAPH from RUNS, MARKERS and its START..END range."
  (when-let* ((matches (seq-filter (lambda (value)
                                     (and (eq (plist-get value :type) 'list-continuation)
                                          start end (< (plist-get value :start) end) (> (plist-get value :end) start))) markers))
              (marker (car matches)))
    (unless (= (length matches) 1) (user-error "Overlapping list continuation annotations require review"))
    (when (alist-get 'bullet paragraph)
      (user-error "A list continuation cannot also have a native bullet"))
    (let ((data (or (plist-get marker :data) "")))
      (unless (string-match "\\`v1:\\(bullet\\|number\\|check\\):\\([0-9]+\\)\\'" data)
        (user-error "Malformed or unsupported list continuation metadata"))
      (let* ((info (list :type (intern (match-string 1 data)) :level (string-to-number (match-string 2 data))))
             (element (gdocs-convert-paragraph-from-docs paragraph runs markers start end 'normal
                                                         (list :list-continuation info))))
        (plist-put element :list-continuation-range-ids (list (plist-get marker :range-id)))
        element))))

(defun gdocs-convert-list-validate-native-marker (type ranges entry)
  "Validate continuation TYPE RANGES in named-range ENTRY."
  (when (eq type 'list-continuation)
    (unless (and (sequencep ranges) (= (length ranges) 1))
      (user-error "A list continuation needs one contiguous native range"))
    (let ((range (elt ranges 0)) (id (alist-get 'namedRangeId entry)))
      (unless (and (stringp id) (not (string-empty-p id))
                   (integerp (alist-get 'startIndex range)) (> (alist-get 'startIndex range) 0)
                   (integerp (alist-get 'endIndex range)) (> (alist-get 'endIndex range) (alist-get 'startIndex range))
                   (not (alist-get 'segmentId range)))
        (user-error "Malformed list continuation range or unsupported document segment")))))

(defun gdocs-convert-list-validate-native-context (content markers)
  "Require continuation MARKERS to belong to one body paragraph in CONTENT."
  (dolist (marker markers)
    (when (eq (plist-get marker :type) 'list-continuation)
      (let ((paragraphs (seq-filter (lambda (element)
                                      (and (alist-get 'paragraph element)
                                           (< (alist-get 'startIndex element) (plist-get marker :end))
                                           (> (alist-get 'endIndex element) (plist-get marker :start)))) content)))
        (unless (and (= (length paragraphs) 1)
                     (>= (plist-get marker :start) (alist-get 'startIndex (car paragraphs)))
                     (<= (plist-get marker :end) (alist-get 'endIndex (car paragraphs))))
          (user-error "List continuation annotation crosses paragraphs or a table; restore its paragraph boundaries before pulling"))))))

(defun gdocs-convert-list-validate-body (ir)
  "Validate continuation owners in decoded body IR and return IR."
  (when (seq-some #'gdocs-convert-list-continuation-p ir)
    (gdocs-convert--assign-list-counters ir))
  ir)

(defun gdocs-convert-list-compact-p (element)
  "Return non-nil when ELEMENT is a compact primary list paragraph."
  (plist-get element :list))

(defun gdocs-convert-list-boundary-p (previous element)
  "Return non-nil when ELEMENT starts a list after non-list PREVIOUS."
  (and (gdocs-convert-list-compact-p element) (not (gdocs-convert-list-compact-p previous))))

(provide 'gdocs-convert-lists)
;;; gdocs-convert-lists.el ends here
