;;; gdocs-convert.el --- Bidirectional org <-> Google Docs format conversion -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gdocs contributors
;; Keywords: docs, org
;; Package-Requires: ((emacs "29.1") (dash "2.19") (s "1.13") (org "9.6"))

;;; Commentary:

;; Bidirectional conversion between org-mode and Google Docs JSON via an
;; intermediate representation (IR).  All conversion passes through a flat
;; list of IR element plists, which serves as the canonical interchange
;; format shared with the diff engine.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'dash)
(require 's)
(require 'org)
(require 'org-element)
(require 'ox)
(require 'url-util)
(require 'gdocs-styles)
(require 'gdocs-style-resolve)
(require 'gdocs-convert-text)
(require 'gdocs-link-resolve)
(require 'gdocs-convert-inline-codec)
(require 'gdocs-convert-dates)
(require 'gdocs-convert-paragraphs)
(require 'gdocs-convert-paragraph-codec)
(require 'gdocs-convert-tables)
(require 'gdocs-convert-images)
(require 'gdocs-convert-lists)
(require 'gdocs-convert-code-blocks)
(require 'gdocs-convert-quotes)
(require 'gdocs-convert-callouts)
(require 'gdocs-convert-tasks)
(require 'gdocs-convert-org-blocks)
(require 'gdocs-convert-headings)
(require 'gdocs-convert-status)
(require 'gdocs-convert-footnotes)

;; ---------------------------------------------------------------------------
;;; ID generation

(defvar gdocs-convert--id-counter 0
  "Running counter for element ID generation.")

(defun gdocs-convert--next-id ()
  "Return the next unique element ID string."
  (cl-incf gdocs-convert--id-counter)
  (format "elem-%03d" gdocs-convert--id-counter))

(defun gdocs-convert--reset-ids ()
  "Reset the element ID counter to zero."
  (setq gdocs-convert--id-counter 0))

;; ---------------------------------------------------------------------------
;;; Semantic feature configuration

;; ---------------------------------------------------------------------------
;;; Public API: org -> IR

(defun gdocs-convert-org-buffer-to-ir ()
  "Parse the current org buffer and return an IR element list.
Uses `org-element-parse-buffer' to obtain the AST, then walks it
to produce the intermediate representation."
  (gdocs-convert--reset-ids)
  (let* ((ast (org-element-parse-buffer))
         (title (gdocs-convert--extract-title ast))
         (body-elements (gdocs-convert--walk-org-data ast)))
    (if title
        (cons title body-elements)
      body-elements)))

(defun gdocs-convert-org-string-to-ir (string)
  "Convert an org-mode STRING to IR.
Creates a temporary buffer, inserts STRING, and calls
`gdocs-convert-org-buffer-to-ir'."
  (with-temp-buffer
    (insert string)
    (org-mode)
    (gdocs-convert-org-buffer-to-ir)))

(defun gdocs-convert--extract-title (ast)
  "Extract the #+TITLE keyword from AST and return an IR element, or nil."
  (let ((title-kw (org-element-map ast 'keyword
                    (lambda (kw)
                      (when (string= (org-element-property :key kw) "TITLE")
                        (org-element-property :value kw)))
                    nil t)))
    (when title-kw
      (list :type 'paragraph
            :style 'title
            :style-name 'gdocs-title
            :contents (list (gdocs-convert--make-plain-run title-kw))
            ;; :source distinguishes synthetic elements created from
            ;; document metadata vs. body paragraphs.  Downstream code
            ;; (e.g. `gdocs-sync--body-start-index') uses this to exclude
            ;; metadata titles from UTF-16 index computation.
            :source 'metadata
            :id (gdocs-convert--next-id)))))

;; ---------------------------------------------------------------------------
;;; Segmented org -> IR (for three-way merge)

(defun gdocs-convert-org-buffer-to-segments ()
  "Parse the current org buffer into segments for three-way merge.
Each segment pairs an IR element with its org text region,
including non-IR text like property drawers that follows the
element.  Returns a plist with:
  :ir        -- the full IR element list (including title)
  :segments  -- list of segment plists, each with :ir-element and :org-text
  :preamble  -- buffer text before the first body IR element
  :postamble -- buffer text after the last IR element"
  (gdocs-convert--reset-ids)
  (let* ((ast (org-element-parse-buffer))
         (title (gdocs-convert--extract-title ast))
         (entries (gdocs-convert--walk-org-data-positioned ast))
         (ir (mapcar (lambda (e) (plist-get e :ir)) entries))
         (full-ir (if title (cons title ir) ir))
         (postamble-start (gdocs-convert--find-postamble-start))
         (segments nil)
         (len (length entries)))
    ;; Preamble: text before the first body element
    (let ((first-pos (if entries
                         (plist-get (car entries) :begin)
                       postamble-start)))
      ;; Build segments: each segment's text runs from its :begin
      ;; to the next entry's :begin (or postamble-start for the last)
      (dotimes (i len)
        (let* ((entry (nth i entries))
               (begin (plist-get entry :begin))
               (end (if (< (1+ i) len)
                        (plist-get (nth (1+ i) entries) :begin)
                      postamble-start))
               (org-text (buffer-substring-no-properties begin end)))
          (push (list :ir-element (plist-get entry :ir)
                      :org-text org-text)
                segments)))
      (list :ir full-ir
            :segments (nreverse segments)
            :preamble (buffer-substring-no-properties (point-min) first-pos)
            :postamble (buffer-substring-no-properties
                        postamble-start (point-max))))))

(defun gdocs-convert--find-postamble-start ()
  "Find the buffer position where the postamble begins.
With property-drawer storage there is no postamble; returns
`point-max'."
  (point-max))

(defun gdocs-convert--walk-org-data-positioned (ast)
  "Walk AST and return IR elements with buffer positions.
Like `gdocs-convert--walk-org-data' but each entry is a plist
with :ir (the IR element) and :begin (buffer position).
The result is sorted by buffer position."
  (let ((result nil))
    (org-element-map ast '(headline paragraph plain-list table
                                    horizontal-rule keyword src-block
                                    example-block quote-block center-block
                                    verse-block special-block fixed-width
                                    latex-environment footnote-definition
                                    drawer)
                     (lambda (el)
                       (let ((parent-type (org-element-type (org-element-parent el))))
                         (pcase (org-element-type el)
                           ('headline
                            (push (list :ir (gdocs-convert--headline-to-ir el)
                                        :begin (org-element-property :begin el))
                                  result))
                           ('paragraph
                            (when (gdocs-convert--top-level-paragraph-p el)
                              (let ((begin (org-element-property :begin el)))
                                (dolist (ir (gdocs-convert--paragraph-to-ir el))
                                  (push (list :ir ir :begin begin) result)))))
                           ('plain-list
                            (dolist (entry (gdocs-convert-list-source-positioned el))
                              (push entry result)))
                           ('table
                            (when (eq (org-element-property :type el) 'org)
                              (push (list :ir (gdocs-convert--table-to-ir el)
                                          :begin (org-element-property :begin el))
                                    result)))
                           ('horizontal-rule
                            (push (list :ir (list :type 'horizontal-rule
                                                  :id (gdocs-convert--next-id))
                                        :begin (org-element-property :begin el))
                                  result))
                           ('quote-block
                            (when (not (eq parent-type 'item))
                              (push (list :ir (gdocs-convert--quote-block-to-ir el)
                                          :begin (org-element-property :begin el))
                                    result)))
                           ('src-block
                            (push (list :ir (gdocs-convert--src-block-to-ir el)
                                        :begin (org-element-property :begin el))
                                  result))
                           ('example-block
                            (push (list :ir (gdocs-convert--example-block-to-ir el)
                                        :begin (org-element-property :begin el))
                                  result))
                           ('keyword
                            (when (gdocs-convert--preservable-keyword-p el)
                              (push (list :ir (gdocs-convert--keyword-to-ir el)
                                          :begin (org-element-property :begin el))
                                    result)))
                           ('center-block
                            (when (not (eq parent-type 'item))
                              (let ((begin (org-element-property :begin el)))
                                (dolist (ir-elem (gdocs-convert--center-block-to-ir el))
                                  (push (list :ir ir-elem :begin begin) result)))))
                           ('verse-block
                            (when (not (eq parent-type 'item))
                              (push (list :ir (gdocs-convert--verse-block-to-ir el)
                                          :begin (org-element-property :begin el))
                                    result)))
                           ('special-block
                            (when (not (eq parent-type 'item))
                              (let ((begin (org-element-property :begin el)))
                                (dolist (ir-elem (gdocs-convert--special-block-to-ir el))
                                  (push (list :ir ir-elem :begin begin) result)))))
                           ('fixed-width
                            (let ((begin (org-element-property :begin el)))
                              (dolist (ir-elem (gdocs-convert--fixed-width-to-ir el))
                                (push (list :ir ir-elem :begin begin) result))))
                           ('latex-environment
                            (let ((begin (org-element-property :begin el)))
                              (dolist (ir-elem (gdocs-convert--latex-environment-to-ir el))
                                (push (list :ir ir-elem :begin begin) result))))
                           ('footnote-definition
                            (push (list :ir (gdocs-convert--footnote-definition-to-ir el)
                                        :begin (org-element-property :begin el))
                                  result))
                           ('drawer
                            (push (list :ir (gdocs-convert--drawer-to-ir el)
                                        :begin (org-element-property :begin el))
                                  result))
                           (_ nil)))))
    (sort (nreverse result)
          (lambda (a b) (< (plist-get a :begin) (plist-get b :begin))))))

(defun gdocs-convert--walk-org-data (ast)
  "Walk an org-element AST and return a flat list of IR elements."
  (mapcar (lambda (entry) (plist-get entry :ir))
          (gdocs-convert--walk-org-data-positioned ast)))

(defun gdocs-convert--top-level-paragraph-p (paragraph)
  "Return non-nil if PARAGRAPH is a direct section child.
A paragraph inside a list item is handled by the list walker, and
a paragraph inside a quote block is handled by the quote walker."
  (let ((parent-type (org-element-type (org-element-parent paragraph))))
    (or (eq parent-type 'section)
        (eq parent-type 'org-data))))

(defun gdocs-convert--preservable-keyword-p (keyword)
  "Return non-nil if KEYWORD should be emitted into Google Docs content.
Document metadata such as AUTHOR, DATE, EMAIL, and LANGUAGE is intentionally
kept in Org source only.  Google Docs has no invisible document-level metadata
slot that round-trips through body sync, so emitting these keywords as body
paragraphs is more surprising than preserving them remotely."
  (ignore keyword)
  nil)

;; ---------------------------------------------------------------------------
;;; Headline -> IR

;; ---------------------------------------------------------------------------
;;; Paragraph -> IR

(defun gdocs-convert--paragraph-to-ir (paragraph)
  "Compose source PARAGRAPH conversion from feature adapters and plain runs."
  (or (gdocs-convert-image-paragraph-to-ir paragraph)
      (let ((runs (gdocs-convert-paragraph-org-runs paragraph)))
        (or (gdocs-convert-code-paragraph-to-ir runs)
            (gdocs-convert-block-paragraph-to-ir paragraph runs)
            (when-let* ((ir (gdocs-convert-paragraph-from-runs
                             runs nil '(:style-name gdocs-body))))
              (list ir))))))

(defun gdocs-convert--trim-runs (runs)
  "Trim leading and trailing whitespace from RUNS.
Drops whitespace-only runs at the start and end, and trims
leading whitespace from the first substantive run and trailing
whitespace from the last.  Returns nil if no text remains."
  (let ((trimmed (gdocs-convert--drop-edge-whitespace-runs runs)))
    (when trimmed
      (gdocs-convert--trim-first-and-last-runs trimmed))))

(defun gdocs-convert--drop-edge-whitespace-runs (runs)
  "Drop whitespace-only runs from the start and end of RUNS."
  (cl-labels ((blank (run)
                (and (string-blank-p (plist-get run :text))
                     (not (plist-get (gdocs-convert--inline-run-policy run) :preserve-whitespace)))))
             (let ((result (copy-sequence runs)))
               (while (and result (blank (car result))) (setq result (cdr result)))
               (setq result (nreverse result))
               (while (and result (blank (car result))) (setq result (cdr result)))
               (nreverse result))))

(defun gdocs-convert--trim-first-and-last-runs (runs)
  "Trim leading whitespace from first run and trailing from last in RUNS."
  (when runs
    (let* ((first (car runs))
           (first-text (if (plist-get (gdocs-convert--inline-run-policy first) :preserve-whitespace)
                           (plist-get first :text) (s-trim-left (plist-get first :text))))
           (last-elt (car (last runs)))
           (last-text (let ((text (if (eq first last-elt) first-text (plist-get last-elt :text))))
                        (if (plist-get (gdocs-convert--inline-run-policy last-elt) :preserve-whitespace)
                            text (s-trim-right text)))))
      (when (and (string-empty-p first-text)
                 (not (plist-get (gdocs-convert--inline-run-policy first) :preserve-whitespace)))
        (setq runs (cdr runs))
        (when runs
          (setq first (car runs))
          (setq first-text (s-trim-left (plist-get first :text)))))
      (when runs
        (setcar runs (plist-put (copy-sequence first) :text first-text))
        (when (and (string-empty-p last-text)
                   (not (plist-get (gdocs-convert--inline-run-policy last-elt) :preserve-whitespace)))
          (setq runs (butlast runs))
          (when runs
            (setq last-elt (car (last runs)))
            (setq last-text (s-trim-right (plist-get last-elt :text)))))
        (when runs
          (setcar (last runs) (plist-put (copy-sequence last-elt)
                                         :text last-text))))
      runs)))

;; ---------------------------------------------------------------------------
;;; Objects -> text runs

(defun gdocs-convert--typed-object-to-runs (object inherited-props)
  "Compose semantic adapters and ordinary rich text for OBJECT and INHERITED-PROPS."
  (or (gdocs-convert-status-source-object object inherited-props)
      (gdocs-convert-footnote-source-object object inherited-props)
      (gdocs-convert-date-source-object object inherited-props)
      (gdocs-convert-code-source-object object inherited-props)
      (gdocs-convert-inline-plain-object object inherited-props)))

(defun gdocs-convert--inline-element-parts (element markers)
  "Compose native ELEMENT decoding with semantic MARKERS."
  (gdocs-convert-code-native-parts
   element markers
   (or (gdocs-convert-status-native-parts element markers)
       (gdocs-convert-footnote-native-parts element markers)
       (gdocs-convert-date-native-parts element markers)
       (gdocs-convert-status-unsupported-native element markers))))

(defun gdocs-convert--inline-finalize-runs (runs)
  "Compose feature-owned completion of reconstructed RUNS."
  (gdocs-convert-code-finalize-runs (gdocs-convert-status-finalize-runs runs)))

(defun gdocs-convert--inline-run-policy (run)
  "Compose feature-owned normalization and addressing policy for RUN."
  (append (gdocs-convert-list-continuation-run-policy run)
          (gdocs-convert-task-run-policy run)
          (gdocs-convert-code-run-policy run)
          (gdocs-convert-date-run-policy run)
          (gdocs-convert-status-run-policy run)
          (gdocs-convert-footnote-run-policy run)))

;; ---------------------------------------------------------------------------
;;; Lists -> IR

;; ---------------------------------------------------------------------------
;;; Quote blocks -> IR

;; ---------------------------------------------------------------------------
;;; Src blocks / keywords -> IR (org-only markers)

(defun gdocs-convert--keyword-to-ir (keyword)
  "Convert a preservable org KEYWORD to an IR element with a marker."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (list :type 'paragraph
          :style 'normal
          :contents (list (gdocs-convert--make-plain-run
                           (format "#+%s: %s" key value)))
          :gdocs-marker (list (list :type 'keyword
                                    :data (list :key key :value value)))
          :id (gdocs-convert--next-id))))

;; ---------------------------------------------------------------------------
;;; Center blocks -> IR

(defun gdocs-convert--center-block-to-ir (center-block)
  "Convert a CENTER-BLOCK element to IR elements with center alignment."
  (let ((result nil))
    (org-element-map center-block 'paragraph
      (lambda (para)
        (let* ((runs (gdocs-convert--objects-to-runs
                      (org-element-contents para)))
               (trimmed (gdocs-convert--trim-runs runs)))
          (when trimmed
            (push (list :type 'paragraph
                        :style 'normal
                        :alignment 'center
                        :contents trimmed
                        :id (gdocs-convert--next-id))
                  result)))))
    (nreverse result)))

;; ---------------------------------------------------------------------------
;;; Verse blocks -> IR

(defun gdocs-convert--verse-block-to-ir (verse-block)
  "Convert a VERSE-BLOCK element to an IR element with a marker."
  (let* ((runs (gdocs-convert--objects-to-runs
                (org-element-contents verse-block)))
         (trimmed (gdocs-convert--trim-runs runs)))
    (list :type 'paragraph
          :style 'normal
          :contents (or trimmed (list (gdocs-convert--make-plain-run "")))
          :gdocs-marker (list (list :type 'verse-block))
          :id (gdocs-convert--next-id))))

;; ---------------------------------------------------------------------------
;;; Special blocks -> IR

(defun gdocs-convert--special-block-to-ir (special-block)
  "Convert a SPECIAL-BLOCK element to IR elements with a marker."
  (or (gdocs-convert-callout-source-block special-block)
    (let ((block-type (org-element-property :type special-block))
          (result nil))
      (org-element-map special-block 'paragraph
        (lambda (para)
          (let* ((runs (gdocs-convert--objects-to-runs
                        (org-element-contents para)))
                 (trimmed (gdocs-convert--trim-runs runs)))
            (when trimmed
              (push (list :type 'paragraph
                          :style 'normal
                          :contents trimmed
                          :gdocs-marker (list (list :type 'special-block
                                                    :data block-type))
                          :id (gdocs-convert--next-id))
                    result)))))
      (nreverse result))))

;; ---------------------------------------------------------------------------
;;; Fixed-width -> IR

(defun gdocs-convert--fixed-width-to-ir (fixed-width)
  "Convert a FIXED-WIDTH element to monospace paragraph IR elements."
  (let* ((value (or (org-element-property :value fixed-width) ""))
         (lines (split-string (s-trim-right value) "\n")))
    (gdocs-convert--lines-to-monospace-ir lines)))

;; ---------------------------------------------------------------------------
;;; LaTeX environments -> IR

(defun gdocs-convert--latex-environment-to-ir (latex-env)
  "Convert a LATEX-ENV element to IR elements with a marker."
  (let* ((value (or (org-element-property :value latex-env) ""))
         (lines (split-string (s-trim-right value) "\n")))
    (mapcar (lambda (line)
              (list :type 'paragraph
                    :style 'normal
                    :contents (list (gdocs-convert--make-run
                                    line (gdocs-convert--code-props)))
                    :gdocs-marker (list (list :type 'latex-environment
                                              :data value))
                    :id (gdocs-convert--next-id)))
            lines)))

;; ---------------------------------------------------------------------------
;;; Footnote definitions -> IR

;;; Drawers -> IR

(defun gdocs-convert--drawer-to-ir (drawer)
  "Convert a DRAWER element to an IR element preserving raw content."
  (let* ((name (org-element-property :drawer-name drawer))
         (raw (buffer-substring-no-properties
               (org-element-property :begin drawer)
               (org-element-property :end drawer))))
    (list :type 'paragraph
          :style 'normal
          :contents (list (gdocs-convert--make-plain-run
                           (s-trim raw)))
          :gdocs-marker (list (list :type 'drawer
                                    :data (list :name name
                                                :raw raw)))
          :id (gdocs-convert--next-id))))

;; ---------------------------------------------------------------------------
;;; Public API: IR -> org

(defun gdocs-convert--split-footnote-elements (ir)
  "Split IR into ordinary body elements and footnote definition elements."
  (let ((body nil)
        (footnotes nil))
    (dolist (element ir)
      (if (eq (plist-get element :type) 'footnote)
          (push element footnotes)
        (push element body)))
    (list :body (nreverse body)
          :footnotes (nreverse footnotes))))

(defun gdocs-convert--append-footnotes-section (org-text footnotes)
  "Append FOOTNOTES as a conventional Org footnotes section to ORG-TEXT."
  (if (null footnotes)
      org-text
    (let ((body (s-trim-right org-text))
          (definitions (mapconcat #'gdocs-convert--ir-footnote-to-org
                                  footnotes "\n\n")))
      (concat body "\n\n* Footnotes\n\n" definitions "\n"))))

(defun gdocs-convert-ir-to-org (ir)
  "Convert an IR element list to an org-mode string.
Returns a well-formatted org string.  Consecutive all-code
paragraphs (monospace lines without a src-block marker) are
grouped into a single =#+BEGIN_EXAMPLE= block.  Footnote definitions are
collected at the end under a conventional `* Footnotes' section."
  (let* ((split (gdocs-convert--split-footnote-elements ir))
         (footnotes (plist-get split :footnotes))
         (parts nil)
         (prev-element nil)
         (prev-type nil)
         (elements (seq-into (plist-get split :body) 'vector))
         (i 0)
         (len (length (plist-get split :body))))
    (gdocs-convert--assign-list-counters (plist-get split :body))
    (while (< i len)
      (let ((element (aref elements i)) group)
        (cond
         ;; Empty paragraph (nil contents): skip in org output but
         ;; preserve in IR for correct index computation.
         ((and (eq (plist-get element :type) 'paragraph)
               (null (plist-get element :contents)))
          (setq i (1+ i)))
         ;; Feature-owned grouping supplies text and the following position.
         ((setq group (gdocs-convert-code-block-org-group elements i))
          (when (and parts prev-type (plist-get group :blank-before))
            (push "" parts))
          (push (plist-get group :org-text) parts)
          (setq i (plist-get group :next-index)
                prev-element (plist-get group :element)
                prev-type (plist-get prev-element :type)))
         ;; Normal element
         (t
          (let ((type (plist-get element :type)))
            (let ((needs-blank (gdocs-convert--needs-blank-line-p
                                prev-element element))
                  (org-text (gdocs-convert--ir-element-to-org element)))
              (when (and needs-blank parts)
                (push "" parts))
              (push org-text parts)
              (setq prev-element element)
              (setq prev-type type)))
          (setq i (1+ i))))))
    (gdocs-convert--append-footnotes-section
     (concat (s-join "\n" (nreverse parts)) "\n")
     footnotes)))

(defun gdocs-convert--plain-text (element)
  "Return the concatenated plain text of ELEMENT's runs."
  (mapconcat (lambda (r) (plist-get r :text))
             (plist-get element :contents) ""))

(defun gdocs-convert--needs-blank-line-p (previous element)
  "Return non-nil if a blank line is needed before ELEMENT.
PREVIOUS is the previous IR element.  Separate semantic blocks and list
boundaries while keeping consecutive list items compact.  Keep a blank line
after headings even when a section starts with a list, so a later parse cannot
merge the heading text into that list item."
  (when previous
    (let ((previous-type (plist-get previous :type))
          (previous-style (plist-get previous :style))
          (type (plist-get element :type))
          (compact (gdocs-convert-list-compact-p element)))
      (or (gdocs-convert-list-continuation-p element)
          (gdocs-convert-callout-element-p previous)
          (gdocs-convert-callout-element-p element)
          (gdocs-convert-quote-element-p previous)
          (gdocs-convert-quote-element-p element)
          (gdocs-convert-code-block-element-p previous)
          (gdocs-convert-code-block-element-p element)
          (gdocs-convert-list-boundary-p previous element)
          (and (eq previous-type 'paragraph)
               (gdocs-convert--heading-style-p previous-style))
          (eq type 'table)
          (eq type 'horizontal-rule)
          (eq type 'footnote)
          (gdocs-convert-image-element-p element)
          (and (eq type 'paragraph)
               (not compact)
               (not (eq previous-type 'horizontal-rule)))))))

(defun gdocs-convert--ir-element-to-org (element)
  "Convert a single IR ELEMENT to an org string."
  (pcase (plist-get element :type)
    ('paragraph (gdocs-convert--ir-paragraph-to-org element))
    ('table (gdocs-convert--ir-table-to-org element))
    ('horizontal-rule "-----")
    ('footnote (gdocs-convert--ir-footnote-to-org element))
    ('page-break "")
    ('image (gdocs-convert--ir-image-to-org element))
    ('source-block (gdocs-convert--ir-source-block-to-org element))
    ('example-block (gdocs-convert--ir-example-block-to-org element))
    ('quote-block (gdocs-convert--ir-quote-block-to-org element))
    ('callout-block (gdocs-convert--ir-callout-block-to-org element))))

(defun gdocs-convert--ir-paragraph-to-org (element)
  "Compose paragraph ELEMENT rendering with ordered feature-owned adapters."
  (gdocs-convert-paragraph-to-org
   element '(gdocs-convert-list-paragraph-to-org
             gdocs-convert-heading-paragraph-to-org
             gdocs-convert-block-paragraph-to-org)))

(defun gdocs-convert--extract-marker-field (marker type-sym)
  "Extract the :data value for TYPE-SYM from MARKER.
MARKER is a list of marker plists."
  (when marker
    (cl-loop for m in marker
             when (eq (plist-get m :type) type-sym)
             return (plist-get m :data))))

;; ---------------------------------------------------------------------------
;;; Text runs -> org text

(defun gdocs-convert--run-to-org (run)
  "Compose feature-owned RUN rendering with ordinary rich text."
  (or (gdocs-convert-code-render-run run)
      (gdocs-convert-status-render-run run)
      (gdocs-convert-footnote-run-to-org run)
      (gdocs-convert-inline-plain-run-to-org run)))

;; ---------------------------------------------------------------------------
;;; IR footnote -> org

;; ---------------------------------------------------------------------------
;;; Public API: Google Docs JSON -> IR

(defun gdocs-convert-docs-json-to-ir (json)
  "Convert Google Docs JSON (alist) to an IR element list.
JSON is an alist as returned by the Google Docs API, with keys
like `body', `title', `lists', and `namedRanges'."
  (gdocs-convert--reset-ids)
  (let* ((title (alist-get 'title json))
         (body (alist-get 'body json))
         (content (alist-get 'content body))
         (lists-map (alist-get 'lists json))
         (named-ranges (alist-get 'namedRanges json))
         (inline-objects (alist-get 'inlineObjects json))
         (markers (gdocs-convert--parse-named-range-markers named-ranges))
         (footnote-refs nil)
         (result nil))
    (gdocs-convert-task-validate-native-context content markers)
    (gdocs-convert-list-validate-native-context content markers)
    (when title
      (push (list :type 'paragraph
                  :style 'title
                  :contents (list (gdocs-convert--make-plain-run title))
                  :source 'metadata ;see `gdocs-convert--org-title-to-ir'
                  :id (gdocs-convert--next-id))
            result))
    (seq-doseq (structural-element content)
      (setq footnote-refs
            (append footnote-refs
                    (gdocs-convert--structural-element-footnote-refs
                     structural-element markers)))
      (let ((ir (gdocs-convert--structural-element-to-ir
                 structural-element lists-map markers inline-objects))
            (si (alist-get 'startIndex structural-element))
            (ei (alist-get 'endIndex structural-element)))
        (when ir
          ;; Some converters (e.g. tables with split rows) return a list
          ;; of IR elements rather than a single one.  Detect the multi-
          ;; element case by checking if (car ir) is itself a typed plist.
          (if (and (listp ir) (listp (car ir)) (plist-get (car ir) :type))
              (progn
                ;; Store doc indices on first sub-element only; for
                ;; tables split into sub-rows the individual ranges
                ;; are not available from the JSON.
                (when (and si (car ir))
                  (plist-put (car ir) :doc-start si)
                  (plist-put (car ir) :doc-end ei))
                (setq result (nconc (nreverse ir) result)))
            ;; Single IR element — store the JSON indices directly.
            (when si
              (plist-put ir :doc-start si)
              (plist-put ir :doc-end ei))
            (push ir result)))))
    (let ((ir (gdocs-convert--attach-callout-block-markers
               (gdocs-convert--attach-quote-block-markers
                (gdocs-convert--attach-source-block-markers
                 (gdocs-convert--attach-example-block-markers
                  (gdocs-convert--attach-image-caption-markers
                   (append (gdocs-convert-list-validate-body (nreverse result))
                           (gdocs-convert--docs-footnotes-to-ir
                            (alist-get 'footnotes json) footnote-refs)))))))))
      (gdocs-convert--assign-list-counters ir)
      ir)))

(defun gdocs-convert--structural-element-to-ir (element lists-map markers
                                                        &optional inline-objects)
  "Convert a Google Docs structural ELEMENT to IR.
LISTS-MAP is the document's lists property for bullet type
lookup.  MARKERS is an alist of element markers from named ranges.
INLINE-OBJECTS is the document's inline object metadata map."
  (cond
   ((alist-get 'paragraph element)
    (let ((paragraph (alist-get 'paragraph element)))
      (or (gdocs-convert--docs-special-paragraph-elements
           paragraph inline-objects markers
           (alist-get 'startIndex element)
           (alist-get 'endIndex element))
          (gdocs-convert--docs-paragraph-to-ir
           paragraph lists-map markers
           (alist-get 'startIndex element)
           (alist-get 'endIndex element)))))
   ((alist-get 'table element)
    (gdocs-convert-status-validate-native-table element markers)
    (gdocs-convert-code-validate-native-table element markers)
    (gdocs-convert--docs-table-to-ir (alist-get 'table element)))
   ((alist-get 'sectionBreak element)
    nil)
   (t nil)))

(defun gdocs-convert--docs-special-paragraph-elements
    (paragraph &optional inline-objects markers start end)
  "Check PARAGRAPH for special elements like page breaks or images.
INLINE-OBJECTS is the document's inline object metadata map.  MARKERS contains
named-range marker data for START to END.  Returns an IR element if found, nil
otherwise."
  (let ((elements (alist-get 'elements paragraph)))
    (catch 'found
      (seq-doseq (el elements)
        (when (alist-get 'pageBreak el)
          (throw 'found (list :type 'page-break
                              :id (gdocs-convert--next-id))))
        (when-let* ((image (gdocs-convert-image-native-element-to-ir
                            el inline-objects markers start end)))
          (throw 'found image)))
      nil)))


;; ---------------------------------------------------------------------------
;;; Google Docs paragraph -> IR

(defun gdocs-convert--markers-for-range (markers start end)
  "Return MARKERS that overlap the Google Docs range START to END."
  (seq-filter (lambda (marker)
                (let ((marker-start (plist-get marker :start))
                      (marker-end (plist-get marker :end)))
                  (and marker-start marker-end start end
                       (< marker-start end)
                       (> marker-end start))))
              markers))

(defun gdocs-convert--docs-paragraph-to-ir (paragraph lists-map markers &optional start end)
  "Compose native PARAGRAPH decoding with LISTS-MAP, MARKERS and START..END."
  (let ((runs (gdocs-convert-paragraph-docs-runs paragraph markers)))
    (or (gdocs-convert-task-paragraph-from-docs paragraph runs markers start end)
        (gdocs-convert-list-paragraph-from-docs paragraph lists-map markers runs start end)
        (gdocs-convert-heading-paragraph-from-docs paragraph runs markers start end)
        (gdocs-convert-paragraph-from-docs
         paragraph runs markers start end 'normal '(:style-name gdocs-body)))))

(defun gdocs-convert--paragraph-markers-for-range (markers start end)
  "Compose paragraph annotations from MARKERS overlapping START..END."
  (gdocs-convert-footnote-paragraph-markers
   (gdocs-convert-task-paragraph-markers
    (gdocs-convert-code-paragraph-markers
     (gdocs-convert--without-inline-status-markers
      (gdocs-convert--markers-for-range markers start end))))))

(defun gdocs-convert--range-marker (markers marker-type start end)
  "Return MARKER-TYPE marker from MARKERS overlapping START..END, or nil."
  (seq-find (lambda (marker)
              (and (eq (plist-get marker :type) marker-type)
                   (let ((marker-start (plist-get marker :start))
                         (marker-end (plist-get marker :end)))
                     (and marker-start marker-end start end
                          (< marker-start end)
                          (> marker-end start)))))
            markers))

;; ---------------------------------------------------------------------------
;;; Named range markers

(defun gdocs-convert--parse-named-range-markers (named-ranges)
  "Parse NAMED-RANGES for gdocs-org-marker entries.
Named ranges encode org-only metadata in the format
\"gdocs-org-marker:TYPE:ID\" or \"gdocs-org-marker:TYPE:ID:DATA\"
where TYPE is the marker kind (e.g. todo, tags) and ID links to
the IR element.  Returns marker plists with range metadata when available."
  (let ((result nil))
    (when named-ranges
      (dolist (entry named-ranges)
        (let* ((name (if (consp entry) (symbol-name (car entry)) ""))
               (named-range (cdr entry))
               (parts (s-split ":" name)))
          (when (and (>= (length parts) 3)
                     (string= (car parts) "gdocs-org-marker"))
            (seq-doseq (range-entry (alist-get 'namedRanges named-range))
              (let* ((ranges (alist-get 'ranges range-entry))
                     (status-p (equal (nth 1 parts) "status"))
                     (range (progn
                              (gdocs-convert-code-validate-native-marker (intern (nth 1 parts)) ranges range-entry)
                              (gdocs-convert-task-validate-native-marker (intern (nth 1 parts)) ranges range-entry)
                              (gdocs-convert-list-validate-native-marker (intern (nth 1 parts)) ranges range-entry)
                              (when (and status-p (/= (length ranges) 1))
                                (user-error "A status marker must have one contiguous range"))
                              (if (vectorp ranges) (aref ranges 0) (car ranges)))))
                (when (and status-p
                           (or (not (stringp (alist-get 'namedRangeId range-entry)))
                               (not (integerp (alist-get 'startIndex range)))
                               (not (integerp (alist-get 'endIndex range)))
                               (let ((segment (alist-get 'segmentId range)))
                                 (and segment (not (string-empty-p segment))))))
                  (user-error "Malformed status marker or unsupported document segment"))
                (push (append (list :type (intern (nth 1 parts))
                                    :id (nth 2 parts)
                                    :data (when (> (length parts) 3)
                                            (s-join ":" (nthcdr 3 parts)))
                                    :start (alist-get 'startIndex range)
                                    :end (alist-get 'endIndex range))
                              (when (alist-get 'namedRangeId range-entry)
                                (list :range-id (alist-get 'namedRangeId range-entry))))
                      result)))))))
    result))

;; ---------------------------------------------------------------------------
;;; Public API: IR -> Google Docs batchUpdate requests

(defun gdocs-convert-ir-to-docs-requests (ir &optional start-index)
  "Convert an IR element list to Google Docs batchUpdate requests.
Returns a list of request alists suitable for
`gdocs-api-batch-update'.  START-INDEX is the UTF-16 index where
the first element begins (default 1)."
  ;; Default 1: Google Docs index 0 is the document root
  (let ((index (or start-index 1))
        (requests nil)
        (element-ranges nil))
    (dolist (element ir)
      (let* ((element (gdocs-convert-list-project-paragraph (gdocs-convert-task-project-paragraph element)))
             (result (gdocs-convert--ir-element-to-requests element index)))
        (push (list :element element :start index
                    :end (plist-get result :index))
              element-ranges)
        (setq requests (nconc requests (plist-get result :requests)))
        (setq index (plist-get result :index))))
    ;; Post-process: merge per-paragraph bullet requests into
    ;; single-range requests per contiguous list group, so Google Docs
    ;; correctly assigns nesting levels from pre-set indentation.
    (gdocs-convert--fixup-list-nesting
     requests (nreverse element-ranges))))

(defun gdocs-convert--ir-element-to-requests (element index)
  "Convert an IR ELEMENT to batchUpdate requests starting at INDEX.
Returns a plist (:requests LIST :index NEW-INDEX)."
  (pcase (plist-get element :type)
    ('paragraph (gdocs-convert--paragraph-to-requests element index))
    ('table (gdocs-convert--table-to-requests element index))
    ('horizontal-rule (gdocs-convert--horizontal-rule-to-requests index))
    ('page-break (gdocs-convert--page-break-to-requests index))
    ('image (gdocs-convert--image-to-requests element index))
    ('source-block (gdocs-convert--source-block-to-requests element index))
    ('example-block (gdocs-convert--example-block-to-requests element index))
    ('quote-block (gdocs-convert--quote-block-to-requests element index))
    ('callout-block (gdocs-convert--callout-block-to-requests element index))
    ('footnote (gdocs-convert--footnote-to-requests element index))))

;; ---------------------------------------------------------------------------
;;; Typographic block -> requests

;; ---------------------------------------------------------------------------
;;; Source block -> requests

;; ---------------------------------------------------------------------------
;;; Image -> requests

;; ---------------------------------------------------------------------------
;;; Paragraph -> requests

(defun gdocs-convert--paragraph-logical-text-request (element start end)
  "Return logical text styling for paragraph ELEMENT between START and END.
Normal paragraphs, including lists, inherit configured body text styling
when they have no explicit logical style.  Inline styles apply afterward."
  (let ((style (or (plist-get element :style-name)
                   (when (eq (plist-get element :style) 'normal)
                     'gdocs-body))))
    (when (and style (< start end))
      (gdocs-convert--logical-style-text-request style start end))))

(defun gdocs-convert--paragraph-to-requests (element index)
  "Convert an IR paragraph ELEMENT to requests starting at INDEX.
Returns a plist (:requests LIST :index NEW-INDEX).

A text-style reset request is emitted after the insertText to
clear any inherited formatting before per-run styles are applied."
  (let* ((element (gdocs-convert-list-project-paragraph (gdocs-convert-task-project-paragraph element)))
         (contents (plist-get element :contents))
         (prefix (gdocs-convert--list-leading-prefix element))
         (prefix-len (gdocs-convert--string-to-utf16-length prefix))
         (text-start (+ index prefix-len))
         (insert-data (gdocs-convert--make-inline-insert-requests
                       contents index prefix))
         (text-len (plist-get insert-data :length))
         (text-only-len (1- text-len))
         (insert-reqs (plist-get insert-data :requests))
         (style-reqs (gdocs-convert--make-paragraph-style-requests
                      element index (+ index text-len)))
         ;; Reset formatting on the text portion (excluding trailing
         ;; newline) so inherited styles are cleared before per-run
         ;; styles are applied.
         (reset-req (when (> text-only-len 0)
                      (gdocs-convert--make-text-style-reset-request
                       text-start (+ index text-only-len))))
         (logical-text-req
          (gdocs-convert--paragraph-logical-text-request
           element text-start (+ index text-only-len)))
         (run-reqs (gdocs-convert--make-text-style-requests
                    contents text-start))
         (list-reqs (gdocs-convert--make-list-requests element index
                                                       (+ index text-len)))
         (marker-reqs (gdocs-convert--make-marker-requests
                       element index (+ index text-len)))
         (footnote-reqs (gdocs-convert--make-footnote-reference-requests
                         contents text-start))
         (all-requests (append insert-reqs
                               style-reqs
                               (when reset-req (list reset-req))
                               (when logical-text-req (list logical-text-req))
                               run-reqs
                               list-reqs
                               marker-reqs
                               footnote-reqs)))
    (list :requests all-requests
          :index (+ index text-len))))

(defun gdocs-convert--make-inline-insert-requests (runs index &optional prefix)
  "Compose semantic and literal insertion plans for RUNS at INDEX with PREFIX."
  (setq prefix (or prefix ""))
  (or (gdocs-convert-date-inline-insert-requests runs index prefix)
      (gdocs-convert-paragraph-text-requests
       (gdocs-convert--runs-to-plain-text runs) index prefix)))

(defun gdocs-convert--logical-paragraph-style-request (element start end)
  "Return logical style paragraph request for ELEMENT between START and END."
  (when-let* ((style-name (plist-get element :style-name)))
    (gdocs-convert--logical-style-paragraph-request style-name start end)))

(defun gdocs-convert--make-paragraph-style-requests (element start end)
  "Create updateParagraphStyle requests for ELEMENT from START to END."
  (let* ((style (plist-get element :style))
         (alignment (plist-get element :alignment))
         (named-style (gdocs-convert--ir-style-to-docs style))
         (logical-req (gdocs-convert--logical-paragraph-style-request
                       element start end))
         (requests nil))
    (when named-style
      (push `((updateParagraphStyle
               . ((paragraphStyle . ((namedStyleType . ,named-style)))
                  (range . ((startIndex . ,start)
                            (endIndex . ,(1- end))))
                  (fields . "namedStyleType"))))
            requests))
    (when alignment
      (let ((docs-alignment (gdocs-convert--ir-alignment-to-docs alignment)))
        (when docs-alignment
          (push `((updateParagraphStyle
                   . ((paragraphStyle . ((alignment . ,docs-alignment)))
                      (range . ((startIndex . ,start)
                                (endIndex . ,(1- end))))
                      (fields . "alignment"))))
                requests))))
    (when logical-req
      (push logical-req requests))
    (nreverse requests)))

(defun gdocs-convert--run-model-length (run)
  "Return Google Docs UTF-16 model length for RUN."
  (or (plist-get (gdocs-convert--inline-run-policy run) :model-length)
      (gdocs-convert--string-to-utf16-length (or (plist-get run :text) ""))))

(defun gdocs-convert-make-text-style-requests (runs start-index &optional segment-id omit-status-markers)
  "Create updateTextStyle requests for formatted RUNS starting at START-INDEX.
Each run that has any formatting generates a request.  When SEGMENT-ID is
non-nil, requests target that document segment, such as a native footnote.
OMIT-STATUS-MARKERS suppresses inline semantic markers emitted
separately by diff."
  (let ((requests nil)
        (index start-index))
    (dolist (run runs)
      (let* ((len (gdocs-convert--run-model-length run))
             (end (+ index len))
             (style-fields (gdocs-convert--run-to-docs-style run)))
        (when (and style-fields (> len 0))
          (push `((updateTextStyle
                   . ((textStyle . ,style-fields)
                      (range . ,(gdocs-convert--text-range
                                 index end segment-id))
                      (fields . ,(gdocs-convert-status-style-fields-mask
                                  run (gdocs-convert--style-fields-mask style-fields))))))
                requests))
        (when-let* ((marker (gdocs-convert-status-run-marker-request
                             run index end segment-id omit-status-markers)))
          (push marker requests))
        (when-let* ((marker (and (not omit-status-markers)
                                 (gdocs-convert-code-run-marker-request run index end segment-id))))
          (push marker requests))
        (unless omit-status-markers
          (dolist (request (append (gdocs-convert-task-run-requests run index segment-id)
                                   (gdocs-convert-list-continuation-run-requests run index segment-id)))
            (push request requests)))
        (setq index end)))
    (nreverse requests)))

(defun gdocs-convert--make-text-style-requests (runs start-index)
  "Create updateTextStyle requests for formatted RUNS starting at START-INDEX."
  (gdocs-convert-make-text-style-requests runs start-index))

(defun gdocs-convert--make-text-style-reset-request (start end)
  "Create an updateTextStyle request that resets formatting from START to END.
Clears bold, italic, underline, strikethrough, weightedFontFamily,
baselineOffset, foregroundColor, and backgroundColor by listing
them in the fields mask with only default-valued properties in the
textStyle body.  Fields present in the mask but absent from the
body are reset to their defaults by the Google Docs API."
  `((updateTextStyle
     . ((textStyle . ((bold . :json-false)
                      (italic . :json-false)
                      (underline . :json-false)
                      (strikethrough . :json-false)
                      (baselineOffset . "NONE")))
        (range . ((startIndex . ,start)
                  (endIndex . ,end)))
        (fields . "bold,italic,underline,strikethrough,weightedFontFamily,baselineOffset,foregroundColor,backgroundColor")))))

(defun gdocs-convert--run-logical-style (run)
  "Return Google Docs TextStyle fields for RUN logical styles."
  (apply #'append
         (mapcar #'gdocs-convert--logical-style-text-fields
                 (plist-get run :style-names))))

(defun gdocs-convert--run-to-docs-style (run)
  "Convert formatting of a text RUN to a Google Docs textStyle alist.
Returns nil if no formatting is applied."
  (let ((style (gdocs-convert-status-run-docs-style run)))
    (when (plist-get run :link)
      (push `(link . ((url . ,(plist-get run :link)))) style))
    (setq style (append (gdocs-convert--run-logical-style run) style))
    (when (plist-get run :strikethrough)
      (push `(strikethrough . t) style))
    (when (plist-get run :underline)
      (push `(underline . t) style))
    (when (plist-get run :italic)
      (push `(italic . t) style))
    (when (plist-get run :bold)
      (push `(bold . t) style))
    (when (plist-get run :superscript)
      (push '(baselineOffset . "SUPERSCRIPT") style))
    (when (plist-get run :subscript)
      (push '(baselineOffset . "SUBSCRIPT") style))
    (when-let ((fg (plist-get run :foreground-color)))
      (push `(foregroundColor . ,(gdocs-convert--hex-to-color fg)) style))
    (when-let ((bg (plist-get run :background-color)))
      (push `(backgroundColor . ,(gdocs-convert--hex-to-color bg)) style))
    (when-let ((size (plist-get run :font-size)))
      (push `(fontSize . ((magnitude . ,size) (unit . "PT"))) style))
    style))

(defun gdocs-convert--make-marker-requests (element start end)
  "Create named range requests for org-only markers in ELEMENT.
START and END define the text range in the document.
ELEMENT's :gdocs-marker is a list of marker plists.
String-valued :data is included in the named range name so it
survives a round-trip through Google Docs.  Empty paragraphs are marked by
covering their newline, because Google Docs rejects zero-length named ranges."
  (let ((marker (plist-get element :gdocs-marker))
        (id (plist-get element :id))
        (range-end (max (1+ start) (1- end))))
    (when (and marker id)
      (mapcar (lambda (m)
                (let* ((marker-type (plist-get m :type))
                       (data (plist-get m :data))
                       (name (if (and data (stringp data))
                                 (format "gdocs-org-marker:%s:%s:%s"
                                         marker-type id data)
                               (format "gdocs-org-marker:%s:%s"
                                       marker-type id))))
                  `((createNamedRange
                     . ((name . ,name)
                        (range . ((startIndex . ,start)
                                  (endIndex . ,range-end))))))))
              marker))))

;; ---------------------------------------------------------------------------
;;; Horizontal rule -> requests

(defun gdocs-convert--horizontal-rule-to-requests (index)
  "Create requests for a horizontal rule at INDEX.
Google Docs has no native horizontal rule element, so we represent
it as a bare newline paragraph.
Returns a plist (:requests LIST :index NEW-INDEX)."
  (let* ((text "\n")
         (len (gdocs-convert--string-to-utf16-length text))
         (insert-req (gdocs-convert--make-insert-text-request text index)))
    (list :requests (list insert-req)
          :index (+ index len))))

(defun gdocs-convert--page-break-to-requests (index)
  "Create requests for a page break at INDEX.
Returns a plist (:requests LIST :index NEW-INDEX)."
  (list :requests (list `((insertPageBreak
                            . ((location . ((index . ,index)))))))
        :index (+ index 1)))

;; ---------------------------------------------------------------------------
;;; UTF-16 helper

(provide 'gdocs-convert)
;;; gdocs-convert.el ends here
