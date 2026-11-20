;;; gdocs-sync-safety.el --- Semantic sync validation -*- lexical-binding: t; -*-

;;; Commentary:
;; Classify semantic IR and compare Org inventories without transport, account
;; selection, backup I/O or buffer replacement.  The sync orchestrator chooses
;; when to validate and how to report failures.  Existing internal entry points
;; remain available for callers and regression tests.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'seq)
(require 'subr-x)

(declare-function gdocs-convert--inline-run-policy "gdocs-convert" (run))

(defun gdocs-sync--semantic-ir-element-p (element)
  "Return non-nil when ELEMENT is unsafe for automatic text merge."
  (let ((type (plist-get element :type)))
    (or (not (eq type 'paragraph))
        (and (eq type 'paragraph)
             (or (not (eq (or (plist-get element :style) 'normal) 'normal))
                 (plist-get element :list)
                 (seq-some (lambda (run)
                             (or (plist-get run :status)
                                 (plist-get (gdocs-convert--inline-run-policy run) :atomic)))
                           (plist-get element :contents)))))))

(defun gdocs-sync--semantic-ir-p (ir)
  "Return non-nil if body IR contains rich semantic structure."
  (seq-some (lambda (element)
              (and (not (eq (plist-get element :style) 'title))
                   (gdocs-sync--semantic-ir-element-p element))) ir))

(defun gdocs-sync--org-semantic-inventory (org-string)
  "Return a semantic safety inventory for ORG-STRING without running user hooks."
  (with-temp-buffer
    (let ((org-inhibit-startup t)) (delay-mode-hooks (org-mode)))
    (insert org-string)
    (let ((tree (org-element-parse-buffer)))
      (list
       :headings
       (org-element-map tree 'headline
                        (lambda (headline)
                          (format "%s:%s" (org-element-property :level headline)
                                  (org-element-property :raw-value headline))))
       :list-structure (org-element-map tree 'item #'gdocs-sync--list-structure-entry)
       :footnote-definitions
       (org-element-map tree 'footnote-definition
                        (lambda (definition) (org-element-property :label definition)))
       :footnote-structure
       (org-element-map tree 'footnote-definition #'gdocs-sync--footnote-structure-entry)
       :tasks
       (org-element-map tree 'item (lambda (item) (org-element-property :checkbox item)))
       :blocks
       (let (counts)
         (dolist (type '(quote-block src-block example-block special-block center-block verse-block))
           (push (cons type 0) counts))
         (org-element-map tree '(quote-block src-block example-block special-block center-block verse-block)
                          (lambda (block)
                            (let ((type (org-element-type block)))
                              (when (or (not (eq type 'special-block))
                                        (string= (org-element-property :type block) "CALLOUT"))
                                (cl-incf (alist-get type counts))))))
         counts)
       :block-structure
       (org-element-map tree '(quote-block src-block example-block special-block center-block verse-block)
                        #'gdocs-sync--org-block-structure-entry)))))

(defun gdocs-sync--list-structure-entry (item)
  "Return a semantic structure entry for Org list ITEM."
  (let* ((lineage (org-element-lineage item))
         (lists (seq-filter (lambda (element) (eq (org-element-type element) 'plain-list)) lineage))
         (parent-list (car lists))
         (type (or (org-element-property :type parent-list) 'unknown))
         (level (max 0 (1- (length lists))))
         (checkbox (or (org-element-property :checkbox item) 'plain)))
    (format "%s@%d:%s" type level checkbox)))

(defun gdocs-sync--footnote-structure-entry (definition)
  "Return a semantic structure entry for footnote DEFINITION."
  (let ((label (org-element-property :label definition)) rich-types)
    (org-element-map definition '(bold italic underline strike-through code verbatim link)
                     (lambda (object) (push (org-element-type object) rich-types)))
    (format "%s:%s" label (mapconcat #'symbol-name (nreverse rich-types) ","))))

(defun gdocs-sync--block-line-count (value)
  "Return the line count for block VALUE."
  (length (split-string (string-trim-right (or value "")) "\n")))

(defun gdocs-sync--org-block-structure-entry (block)
  "Return a semantic structure entry for Org BLOCK, or nil."
  (let ((type (org-element-type block)))
    (pcase type
      ('quote-block
       (format "quote:%d" (length (seq-filter
                                   (lambda (child) (eq (org-element-type child) 'paragraph))
                                   (org-element-contents block)))))
      ('center-block
       (format "center:%d" (length (seq-filter
                                    (lambda (child) (eq (org-element-type child) 'paragraph))
                                    (org-element-contents block)))))
      ('verse-block
       (let* ((begin (org-element-property :contents-begin block))
              (end (org-element-property :contents-end block))
              (value (if (and begin end) (buffer-substring-no-properties begin end) ""))
              (lines (unless (string-empty-p value)
                       (split-string (string-remove-suffix "\n" value) "\n" nil))))
         (format "verse:%S"
                 (mapcar (lambda (line)
                           (list (progn (string-match "\\`[[:blank:]]*" line) (match-string 0 line))
                                 (progn (string-match "[[:blank:]]*\\'" line) (match-string 0 line))
                                 (string-blank-p line))) lines))))
      ('src-block
       (format "src:%s:%d" (or (org-element-property :language block) "")
               (gdocs-sync--block-line-count (org-element-property :value block))))
      ('example-block
       (format "example:%d" (gdocs-sync--block-line-count (org-element-property :value block))))
      ('special-block
       (when (string= (org-element-property :type block) "CALLOUT")
         (format "callout:%d" (length (seq-filter
                                       (lambda (child) (memq (org-element-type child) '(paragraph item plain-list)))
                                       (org-element-contents block)))))))))

(defun gdocs-sync--counts (values)
  "Return a hash table counting VALUES."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (value values table)
      (puthash value (1+ (gethash value table 0)) table))))

(defun gdocs-sync--dropped-values-diagnostics (label old-values new-values)
  "Report values under LABEL lost from OLD-VALUES to NEW-VALUES."
  (let ((old-counts (gdocs-sync--counts old-values))
        (new-counts (gdocs-sync--counts new-values)) diagnostics)
    (maphash (lambda (value old-count)
               (let ((new-count (gethash value new-counts 0)))
                 (when (< new-count old-count)
                   (push (format "%s `%s' count dropped from %d to %d"
                                 label value old-count new-count) diagnostics)))) old-counts)
    diagnostics))

(defun gdocs-sync--introduced-duplicate-heading-diagnostics (old-headings new-headings)
  "Report suspicious heading duplicates in OLD-HEADINGS and NEW-HEADINGS."
  (let ((old-counts (gdocs-sync--counts old-headings))
        (new-counts (gdocs-sync--counts new-headings)) diagnostics)
    (maphash (lambda (heading new-count)
               (let ((old-count (gethash heading old-counts 0)))
                 (when (and (> new-count 1) (> new-count old-count))
                   (push (format "heading `%s' duplicated from %d to %d"
                                 heading old-count new-count) diagnostics)))) new-counts)
    diagnostics))

(defun gdocs-sync--heading-outline-diagnostics (old-headings new-headings)
  "Report differences between authored OLD-HEADINGS and NEW-HEADINGS."
  (unless (equal old-headings new-headings)
    (list (format "heading outline changed from `%s' to `%s'"
                  (string-join old-headings " > ") (string-join new-headings " > ")))))

(defun gdocs-sync--first-sequence-difference (old new)
  "Return the first differing index between OLD and NEW, or nil."
  (let ((index 0) found)
    (while (and (not found) (or old new))
      (unless (equal (car old) (car new)) (setq found index))
      (setq old (cdr old) new (cdr new) index (1+ index)))
    found))

(defun gdocs-sync--format-structure-diff (label old new)
  "Return an actionable structure diff message for LABEL, OLD, and NEW."
  (let ((first-diff (gdocs-sync--first-sequence-difference old new)))
    (format "%s changed: count %d -> %d%s" label (length old) (length new)
            (if first-diff
                (format "; first difference at item %d: `%s' -> `%s'"
                        (1+ first-diff) (or (nth first-diff old) "<missing>")
                        (or (nth first-diff new) "<missing>")) ""))))

(defun gdocs-sync--list-structure-diagnostics (old-list new-list)
  "Report list structure lost from OLD-LIST to NEW-LIST, not additions."
  (gdocs-sync--dropped-values-diagnostics "list structure" old-list new-list))

(defun gdocs-sync--footnote-structure-diagnostics (old-footnotes new-footnotes)
  "Report changed body semantics in OLD-FOOTNOTES and NEW-FOOTNOTES."
  (unless (equal old-footnotes new-footnotes)
    (list (format "footnote structure changed from `%s' to `%s'"
                  (string-join old-footnotes " > ") (string-join new-footnotes " > ")))))

(defun gdocs-sync--block-count-diagnostics (old-blocks new-blocks)
  "Report semantic block count drops from OLD-BLOCKS to NEW-BLOCKS."
  (let (diagnostics)
    (dolist (entry old-blocks diagnostics)
      (let* ((type (car entry)) (old-count (cdr entry))
             (new-count (or (alist-get type new-blocks) 0)))
        (when (< new-count old-count)
          (push (format "%s count dropped from %d to %d" type old-count new-count) diagnostics))))))

(defun gdocs-sync--block-structure-diagnostics (old-blocks new-blocks)
  "Report semantic block shape differences between OLD-BLOCKS and NEW-BLOCKS."
  (let ((old-blocks (delq nil old-blocks)) (new-blocks (delq nil new-blocks)))
    (unless (equal old-blocks new-blocks)
      (list (gdocs-sync--format-structure-diff "block structure" old-blocks new-blocks)))))

(defun gdocs-sync--pull-integrity-diagnostics (old-org new-org)
  "Return semantic integrity diagnostics for replacing OLD-ORG with NEW-ORG."
  (let ((old (gdocs-sync--org-semantic-inventory old-org))
        (new (gdocs-sync--org-semantic-inventory new-org)))
    (append
     (gdocs-sync--heading-outline-diagnostics (plist-get old :headings) (plist-get new :headings))
     (gdocs-sync--introduced-duplicate-heading-diagnostics (plist-get old :headings) (plist-get new :headings))
     (gdocs-sync--list-structure-diagnostics (plist-get old :list-structure) (plist-get new :list-structure))
     (gdocs-sync--dropped-values-diagnostics "footnote definition"
                                             (plist-get old :footnote-definitions) (plist-get new :footnote-definitions))
     (gdocs-sync--footnote-structure-diagnostics (plist-get old :footnote-structure) (plist-get new :footnote-structure))
     (gdocs-sync--dropped-values-diagnostics "task checkbox" (plist-get old :tasks) (plist-get new :tasks))
     (gdocs-sync--block-count-diagnostics (plist-get old :blocks) (plist-get new :blocks))
     (gdocs-sync--block-structure-diagnostics (plist-get old :block-structure) (plist-get new :block-structure)))))

(provide 'gdocs-sync-safety)
;;; gdocs-sync-safety.el ends here
