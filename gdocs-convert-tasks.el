;;; gdocs-convert-tasks.el --- Editable managed task glyphs -*- lexical-binding: t; -*-

;;; Commentary:
;; Org task state is projected as ordinary editable glyph text in Docs.  A
;; named range anchors the separator after the glyph and records nesting; no
;; hidden completion snapshot
;; overrides visible edits.  Native checkbox controls are not inferred as open.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'gdocs-convert-paragraph-codec)

(declare-function gdocs-convert--list-prefixes-before "gdocs-convert" (range element-ranges))

(defconst gdocs-convert-task-indent-pt 36
  "Indentation step for version-one managed task presentation, in points.")

(defun gdocs-convert-task-paragraph-p (element)
  "Return non-nil for semantic task paragraph ELEMENT."
  (and (eq (plist-get element :type) 'paragraph)
       (eq (plist-get (plist-get element :list) :type) 'check)))

(defun gdocs-convert-task--glyph (info)
  "Return the visible version-one task glyph for list INFO."
  (cond ((plist-get info :partial) "◩") ((plist-get info :checked) "☑") (t "☐")))

(defun gdocs-convert-task-project-paragraph (element)
  "Project task ELEMENT into physical paragraph text without modifying semantic IR."
  (if (not (gdocs-convert-task-paragraph-p element)) element
    (let* ((info (plist-get element :list)) (level (or (plist-get info :level) 0))
           (copy (copy-sequence element))
           (glyph (gdocs-convert--make-plain-run (gdocs-convert-task--glyph info)))
           (markers (seq-remove (lambda (marker) (memq (plist-get marker :type) '(list-state task-state managed-task)))
                                (plist-get element :gdocs-marker))))
      (unless (and (integerp level) (>= level 0)) (user-error "Invalid managed task nesting level"))
      (plist-put glyph :managed-task-prefix
                 (list :level level :layout (if (plist-get element :managed-task-legacy-separator) 'space 'tab)))
      (plist-put copy :contents (append (list glyph (gdocs-convert--make-plain-run
                                                     (if (plist-get element :managed-task-legacy-separator) " " "\t")))
                                        (mapcar #'copy-sequence (plist-get element :contents))))
      (plist-put copy :list nil) (plist-put copy :gdocs-marker markers)
      copy)))

(defun gdocs-convert-task-list-info (element)
  "Return projected task ELEMENT's list membership, or nil."
  (when-let* ((info (plist-get (car (plist-get element :contents)) :managed-task-prefix)))
    (list :type 'check :level (plist-get info :level))))

(defun gdocs-convert-task-run-policy (run)
  "Return normalization policy for projected task prefix RUN."
  (when (plist-get run :managed-task-prefix)
    '(:atomic t :preserve-whitespace t :preserve-terminal t :formatted t)))

(defun gdocs-convert-task-run-requests (run index &optional segment-id)
  "Return metadata and indentation requests for projected task RUN at INDEX."
  (when-let* ((info (plist-get run :managed-task-prefix)))
    (when segment-id (user-error "Managed task glyphs require body paragraphs, not document segments"))
    (let* ((level (plist-get info :level)) (legacy (eq (plist-get info :layout) 'space))
           (indent (* (if legacy level (1+ level)) gdocs-convert-task-indent-pt))
           (glyph-indent (if legacy indent (- indent (/ gdocs-convert-task-indent-pt 2)))))
      (list `((createNamedRange
               . ((name . ,(format "gdocs-org-marker:managed-task:task-%d:v1:%d" index level))
                  (range . ((startIndex . ,(1+ index)) (endIndex . ,(+ index 2)))))))
            `((deleteParagraphBullets . ((range . ((startIndex . ,index) (endIndex . ,(1+ index)))))))
            `((updateParagraphStyle
               . ((paragraphStyle . ((indentStart . ((magnitude . ,indent) (unit . "PT")))
                                     (indentFirstLine . ((magnitude . ,glyph-indent) (unit . "PT")))))
                  (range . ((startIndex . ,index) (endIndex . ,(1+ index))))
                  (fields . "indentStart,indentFirstLine"))))))))

(defun gdocs-convert-task-post-bullet-indents (ranges)
  "Restore task indentation after native bullet changes and guards in RANGES."
  (cl-loop for range in ranges
           for run = (car (plist-get (plist-get range :element) :contents))
           when (plist-get run :managed-task-prefix)
           collect (car (last (gdocs-convert-task-run-requests
                               run (- (plist-get range :start)
                                      (gdocs-convert--list-prefixes-before range ranges)))))))

(defun gdocs-convert-task-paragraph-markers (markers)
  "Exclude managed task MARKERS that belong to visible prefixes, not paragraphs."
  (seq-remove (lambda (marker) (eq (plist-get marker :type) 'managed-task)) markers))

(defun gdocs-convert-task-validate-native-marker (type ranges entry)
  "Validate managed task TYPE RANGES in native named-range ENTRY."
  (when (eq type 'managed-task)
    (unless (and (or (listp ranges) (vectorp ranges)) (= (length ranges) 1))
      (user-error "Managed task metadata requires one contiguous glyph range"))
    (let ((range (elt ranges 0)))
      (unless (and (listp range) (stringp (alist-get 'namedRangeId entry))
                   (integerp (alist-get 'startIndex range)) (integerp (alist-get 'endIndex range))
                   (= (1+ (alist-get 'startIndex range)) (alist-get 'endIndex range))
                   (not (alist-get 'segmentId range)))
        (user-error "Malformed managed task glyph range or unsupported document segment")))))

(defun gdocs-convert-task-validate-native-list (paragraph lists-map &optional markers start end)
  "Reject recognized native checkboxes in PARAGRAPH using LISTS-MAP and MARKERS.
An unspecified glyph also occurs in ordinary bullets, so it is not evidence
of checkbox semantics.  Legacy task provenance identifies opaque task lists."
  (when-let* ((bullet (alist-get 'bullet paragraph)))
    (let* ((id (alist-get 'listId bullet))
           (properties (and (stringp id) (alist-get (intern id) lists-map)))
           (levels (alist-get 'nestingLevels (alist-get 'listProperties properties)))
           (level (or (alist-get 'nestingLevel bullet) 0))
           (definition (when (< level (length levels)) (elt levels level)))
           (type (alist-get 'glyphType definition)) (glyph (alist-get 'glyphSymbol definition))
           (legacy-task (seq-some (lambda (marker)
                                    (and (or (eq (plist-get marker :type) 'task-state)
                                             (and (eq (plist-get marker :type) 'list-state)
                                                  (string-prefix-p "check:" (or (plist-get marker :data) ""))))
                                         start end (< (plist-get marker :start) end) (> (plist-get marker :end) start))) markers)))
      (when (or (equal type "CHECKBOX")
                (member glyph '("☐" "☑" "☒" "□" "❏" "❑" "◻" "⬜" "✓" "✔"))
                legacy-task)
        (user-error "Native checkbox state is not readable through the Docs API; convert this list to managed ☐, ☑, ◩ text glyphs before pulling")))))

(defun gdocs-convert-task-validate-native-context (content markers)
  "Require managed task MARKERS to identify body paragraphs in CONTENT."
  (dolist (marker markers)
    (when (eq (plist-get marker :type) 'managed-task)
      (unless (seq-some (lambda (element)
                          (and (alist-get 'paragraph element)
                               (integerp (alist-get 'startIndex element))
                               (= (plist-get marker :start) (1+ (alist-get 'startIndex element)))
                               (integerp (alist-get 'endIndex element))
                               (<= (plist-get marker :end) (alist-get 'endIndex element)))) content)
        (user-error "Managed task metadata must anchor a body paragraph prefix; restore its glyph and space, or move it outside tables")))))

(defun gdocs-convert-task--level (marker paragraph &optional separator)
  "Read nesting from MARKER and PARAGRAPH, accounting for SEPARATOR layout."
  (let ((data (or (plist-get marker :data) "")))
    (unless (string-match "\\`v1:\\([0-9]+\\)\\'" data)
      (user-error "Unsupported managed task metadata"))
    (let* ((saved (string-to-number (match-string 1 data)))
           (indent (alist-get 'magnitude (alist-get 'indentFirstLine (alist-get 'paragraphStyle paragraph))))
           (base (if (eq separator ?\t) (/ gdocs-convert-task-indent-pt 2) 0)))
      (if (null indent) saved
        (unless (and (numberp indent) (>= indent base)
                     (= (mod (- indent base) gdocs-convert-task-indent-pt) 0))
          (user-error "Managed task indentation must retain its glyph column and 36-point nesting steps"))
        (round (/ (- indent base) gdocs-convert-task-indent-pt))))))

(defun gdocs-convert-task--without-prefix (runs)
  "Return cloned RUNS without their visible glyph and separator."
  (let ((copy (mapcar #'copy-sequence runs)) (remaining 2))
    (while (and copy (> remaining 0))
      (let* ((text (or (plist-get (car copy) :text) "")) (take (min remaining (length text))))
        (setq remaining (- remaining take))
        (if (= take (length text)) (setq copy (cdr copy))
          (plist-put (car copy) :text (substring text take)))))
    copy))

(defun gdocs-convert-task-paragraph-from-docs (paragraph runs markers start end)
  "Decode managed task PARAGRAPH from visible RUNS and identifying MARKERS."
  (let ((matches (seq-filter (lambda (marker)
                               (and (eq (plist-get marker :type) 'managed-task)
                                    start end (< (plist-get marker :start) end) (> (plist-get marker :end) start))) markers)))
    (when matches
      (unless (and (= (length matches) 1) (= (plist-get (car matches) :start) (1+ start)))
        (user-error "Managed task marker does not identify the separator after one leading glyph"))
      (when (alist-get 'bullet paragraph)
        (user-error "Managed glyph tasks must not also use native list controls"))
      (let* ((marker (car matches)) (text (gdocs-convert--runs-to-plain-text runs))
             (glyph (and (> (length text) 0) (substring text 0 1)))
             (separator (and (> (length text) 1) (aref text 1)))
             (level (gdocs-convert-task--level marker paragraph separator))
             (checked (equal glyph "☑")) (partial (equal glyph "◩")))
        (unless (and (member glyph '("☐" "☑" "◩")) (>= (length text) 2) (memq separator '(?\s ?\t)))
          (user-error "Managed task needs one visible ☐, ☑ or ◩ followed by its space or tab; restore the glyph before pulling"))
        (let* ((info (list :type 'check :level level :checked checked :partial partial))
               (element (gdocs-convert-paragraph-from-docs
                         paragraph (gdocs-convert-task--without-prefix runs) markers start end 'normal (list :list info)))
               (annotations (seq-remove (lambda (value) (memq (plist-get value :type) '(list-state task-state managed-task)))
                                        (plist-get element :gdocs-marker))))
          (plist-put element :gdocs-marker
                     (append (list (list :type 'list-state :data (format "check:%d" level))
                                   (list :type 'task-state :data (cond (partial "partial") (checked "on") (t "off")))) annotations))
          (plist-put element :managed-task-range-ids (list (plist-get marker :range-id)))
          (when (eq separator ?\s)
            (plist-put element :managed-task-legacy-separator t))
          element)))))

(provide 'gdocs-convert-tasks)
;;; gdocs-convert-tasks.el ends here
