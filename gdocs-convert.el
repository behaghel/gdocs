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
(require 'gdocs-convert-text)
(require 'gdocs-convert-inline-codec)
(require 'gdocs-convert-dates)
(require 'gdocs-convert-paragraphs)
(require 'gdocs-convert-paragraph-codec)
(require 'gdocs-convert-images)
(require 'gdocs-convert-lists)
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
;;; Cross-document link resolution

(defcustom gdocs-link-directories nil
  "Additional directories to scan for cross-document link resolution.
`gdocs-directory' is always included."
  :type '(repeat directory)
  :group 'gdocs)

(defcustom gdocs-callout-types '("info" "note" "warning" "tip" "important")
  "Supported semantic Org callout types."
  :type '(repeat string)
  :group 'gdocs)

(defcustom gdocs-callout-labels
  '(("info" . "ℹ️ Info")
    ("note" . "📝 Note")
    ("warning" . "⚠️ Warning")
    ("tip" . "💡 Tip")
    ("important" . "❗ Important"))
  "Visible labels used for semantic callout types."
  :type '(alist :key-type string :value-type string)
  :group 'gdocs)

(defcustom gdocs-image-caption-style-name "org-image-caption"
  "Semantic style name declared for Org image caption markers.
The converter records this name in caption marker metadata but does not apply
visual styling.  Host adapters or users may use the marker to apply their own
Google Docs styling policy."
  :type 'string
  :group 'gdocs)

(defvar gdocs-convert--link-context nil
  "Plist with context for cross-document link resolution.
When non-nil, file: and id: links to org files with Google Doc
counterparts are resolved to Google Docs URLs during org-to-IR
conversion, and Google Docs URLs are reverse-resolved to org
links during IR-to-org conversion.
Contains :buffer-file and :docid-map.")

(defvar gdocs-convert--document-id nil
  "Document ID of the Google Doc being converted.
Used during JSON-to-IR conversion to resolve internal heading
links, which the Google Docs API returns as headingId rather
than full URLs.")

(defvar gdocs-convert--heading-cache (make-hash-table :test 'equal)
  "Session cache: doc-id -> alist of ((heading-text . heading-id) ...).
Populated during push/pull when we fetch a document's JSON.
Used during org-to-IR link resolution to append heading anchors.")

(defvar gdocs-convert--heading-reverse-cache (make-hash-table :test 'equal)
  "Session cache: doc-id -> alist of ((heading-id . heading-text) ...).
Reverse mapping of `gdocs-convert--heading-cache', used during
IR-to-org conversion to resolve heading anchors back to text.")

(defun gdocs-convert--read-file-property-gdocs-id (file-path)
  "Read GDOCS_DOCUMENT_ID from FILE-PATH's file-level property drawer.
Reads only the head of the file (~1KB) and uses a regexp.
Returns the document ID string or nil."
  (when (and file-path (file-readable-p file-path))
    (let ((head-bytes 1024))
      (with-temp-buffer
        (insert-file-contents file-path nil 0
                              (min head-bytes
                                   (file-attribute-size
                                    (file-attributes file-path))))
        (goto-char (point-min))
        (when (and (looking-at-p ":PROPERTIES:")
                   (re-search-forward
                    "^:GDOCS_DOCUMENT_ID:\\s-+\\(.+\\)$" nil t))
          (string-trim (match-string 1)))))))

(defun gdocs-convert--build-docid-to-file-map (directories)
  "Scan org files in DIRECTORIES and return a doc-id-to-file hash table.
Each key is a Google Docs document ID, each value is an absolute file path."
  (let ((map (make-hash-table :test 'equal)))
    (dolist (dir directories)
      (when (file-directory-p dir)
        (dolist (file (directory-files dir t "\\.org\\'"))
          (when-let* ((doc-id (gdocs-convert--read-file-property-gdocs-id file)))
            (puthash doc-id file map)))))
    map))

(defun gdocs-convert--cache-heading-ids (doc-id json)
  "Extract heading IDs from document JSON and cache them under DOC-ID.
Stores an alist of (heading-text . heading-id) pairs, plus a
reverse alist of (heading-id . heading-text) pairs under
DOC-ID-reverse."
  (let ((body (alist-get 'body json))
        (forward nil)
        (reverse nil))
    (when body
      (seq-doseq (structural-element (alist-get 'content body))
        (let ((paragraph (alist-get 'paragraph structural-element)))
          (when paragraph
            (let* ((style-obj (alist-get 'paragraphStyle paragraph))
                   (named-style (alist-get 'namedStyleType style-obj))
                   (heading-id (alist-get 'headingId style-obj)))
              (when (and heading-id
                         (string-match-p "^HEADING_[1-6]$" named-style))
                (let* ((elements (alist-get 'elements paragraph))
                       (text (gdocs-convert--docs-paragraph-plain-text
                              elements)))
                  (when (and text (not (string-empty-p text)))
                    (push (cons text heading-id) forward)
                    (push (cons heading-id text) reverse)))))))))
    (puthash doc-id (nreverse forward) gdocs-convert--heading-cache)
    (puthash doc-id (nreverse reverse)
             gdocs-convert--heading-reverse-cache)))

(defun gdocs-convert--docs-paragraph-plain-text (elements)
  "Extract plain text from Google Docs paragraph ELEMENTS.
Returns the concatenated text content with trailing whitespace trimmed."
  (let ((parts nil))
    (seq-doseq (el elements)
      (let ((text-run (alist-get 'textRun el)))
        (when text-run
          (push (alist-get 'content text-run) parts))))
    (s-trim-right (apply #'concat (nreverse parts)))))

(defconst gdocs-convert--docs-url-regexp
  "https://docs\\.google\\.com/document/d/\\([^/#]+\\)\\(?:/[^#]*\\)?\\(?:#heading=h\\.\\([^&]+\\)\\)?"
  "Regexp matching a Google Docs document URL.
Group 1 captures the document ID.
Group 2 captures the heading ID (if present).")

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
             (when (gdocs-convert--top-level-list-p el)
               (dolist (entry (gdocs-convert--plain-list-positioned el 0))
                 (push entry result))))
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

(defun gdocs-convert--plain-list-positioned (plain-list depth)
  "Convert PLAIN-LIST to positioned IR entries at nesting DEPTH.
Returns a list of (:ir ELEMENT :begin POS) plists."
  (let ((list-type (gdocs-convert--org-list-type plain-list))
        (result nil))
    (dolist (item (org-element-contents plain-list))
      (when (eq (org-element-type item) 'item)
        (setq result
              (nconc result
                     (gdocs-convert--item-positioned
                      item list-type depth)))))
    result))

(defun gdocs-convert--item-list-type (item fallback-type)
  "Return list type for ITEM, falling back to FALLBACK-TYPE.
A checkbox on one Org item does not make sibling plain bullets into tasks, even
when the containing plain-list uses checklist rendering for mixed items."
  (let ((bullet (or (org-element-property :bullet item) "")))
    (cond
     ((org-element-property :checkbox item) 'check)
     ((string-match-p "\\`[0-9]+[.)][[:space:]]+" bullet) 'number)
     ((string-match-p "\\`[A-Za-z]+[.)][[:space:]]+" bullet) 'number)
     ((eq fallback-type 'check) 'bullet)
     (t fallback-type))))

(defun gdocs-convert--item-positioned (item list-type depth)
  "Convert list ITEM to positioned IR entries.
LIST-TYPE is `bullet', `number', or `check'.  DEPTH is the
0-based nesting level.
Returns a list of (:ir ELEMENT :begin POS) plists."
  (let* ((checkbox (org-element-property :checkbox item))
         (effective-type (gdocs-convert--item-list-type item list-type))
         (checked (eq checkbox 'on))
         (partial (eq checkbox 'trans))
         (paragraph (org-element-map item 'paragraph #'identity nil t))
         (runs (when paragraph
                 (gdocs-convert--strip-continuation-indent
                  (gdocs-convert--objects-to-runs
                   (org-element-contents paragraph)))))
         (trimmed (gdocs-convert--trim-runs runs))
         (list-plist (append (list :type effective-type :level depth)
                             (when (eq effective-type 'check)
                               (list :checked checked :partial partial))))
         (markers (append
                   (list (list :type 'list-state
                               :data (format "%s:%d" effective-type depth)))
                   (when checkbox
                     (list (list :type 'task-state
                                 :data (pcase checkbox
                                         ('on "on")
                                         ('trans "partial")
                                         (_ "off")))))))
         (ir-element (append
                      (list :type 'paragraph
                            :style 'normal
                            :list list-plist
                            :contents (or trimmed
                                          (list (gdocs-convert--make-plain-run "")))
                            :id (gdocs-convert--next-id)
                            :gdocs-marker markers)))
         (result (list (list :ir ir-element
                             :begin (org-element-property :begin item)))))
    (dolist (child (org-element-contents item))
      (when (eq (org-element-type child) 'plain-list)
        (setq result
              (nconc result
                     (gdocs-convert--plain-list-positioned
                      child (1+ depth))))))
    result))

(defun gdocs-convert--top-level-paragraph-p (paragraph)
  "Return non-nil if PARAGRAPH is a direct section child.
A paragraph inside a list item is handled by the list walker, and
a paragraph inside a quote block is handled by the quote walker."
  (let ((parent-type (org-element-type (org-element-parent paragraph))))
    (or (eq parent-type 'section)
        (eq parent-type 'org-data))))

(defun gdocs-convert--top-level-list-p (plain-list)
  "Return non-nil if PLAIN-LIST is a direct document/section child."
  (let ((parent-type (org-element-type (org-element-parent plain-list))))
    (or (eq parent-type 'section)
        (eq parent-type 'org-data))))

(defun gdocs-convert--block-paragraph-runs (block)
  "Return non-empty paragraph run lists from Org BLOCK."
  (let (paragraphs)
    (org-element-map block 'paragraph
      (lambda (paragraph)
        (let* ((runs (gdocs-convert--objects-to-runs
                      (org-element-contents paragraph)))
               (trimmed (gdocs-convert--trim-runs runs)))
          (when trimmed
            (push trimmed paragraphs)))))
    (nreverse paragraphs)))

(defun gdocs-convert--reject-nested-block-list-elements (elements block-name)
  "Signal if ELEMENTS contain nested list items inside BLOCK-NAME."
  (when-let* ((nested (seq-find
                       (lambda (element)
                         (let ((list-info (plist-get element :list)))
                           (and list-info
                                (> (or (plist-get list-info :level) 0) 0))))
                       elements)))
    (user-error "Unsupported Org %s nested list item `%s'; Google Docs paragraph shading cannot preserve nested list indentation and full block background in v1"
                block-name
                (gdocs-convert--runs-to-plain-text
                 (plist-get nested :contents)))))

(defun gdocs-convert--block-body-elements (block allowed-types block-name)
  "Return positioned body element IR for direct children of Org BLOCK.
ALLOWED-TYPES is a list of permitted direct child element types.  BLOCK-NAME is
used in diagnostics.  V1 supports paragraph/list container children on this
shared path; unsupported or visually lossy children fail closed before remote
mutation."
  (let (entries)
    (dolist (child (org-element-contents block))
      (unless (stringp child)
        (let ((child-type (org-element-type child)))
          (unless (memq child-type allowed-types)
            (user-error "Unsupported Org %s child `%s'; v1 supports paragraphs and lists only"
                        block-name child-type))
          (pcase child-type
            ('paragraph
             (when-let* ((ir (gdocs-convert--paragraph-child-to-ir child)))
               (push (list :ir ir
                           :begin (org-element-property :begin child))
                     entries)))
            ('plain-list
             (setq entries
                   (nconc (gdocs-convert--plain-list-positioned child 0)
                          entries)))))))
    (let ((elements (mapcar (lambda (entry) (plist-get entry :ir))
                            (sort entries
                                  (lambda (a b) (< (plist-get a :begin)
                                                   (plist-get b :begin)))))))
      (gdocs-convert--reject-nested-block-list-elements elements block-name)
      elements)))

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
  (let ((result (cl-loop for r in runs
                         with started = nil
                         when (or started
                                  (not (string-blank-p
                                        (plist-get r :text))))
                         collect r
                         and do (setq started t))))
    (nreverse (cl-loop for r in (nreverse result)
                       with started = nil
                       when (or started
                                (not (string-blank-p
                                      (plist-get r :text))))
                       collect r
                       and do (setq started t)))))

(defun gdocs-convert--trim-first-and-last-runs (runs)
  "Trim leading whitespace from first run and trailing from last in RUNS."
  (when runs
    (let* ((first (car runs))
           (first-text (s-trim-left (plist-get first :text)))
           (last-elt (car (last runs)))
           (last-text (s-trim-right (plist-get last-elt :text))))
      (when (string-empty-p first-text)
        (setq runs (cdr runs))
        (when runs
          (setq first (car runs))
          (setq first-text (s-trim-left (plist-get first :text)))))
      (when runs
        (setcar runs (plist-put (copy-sequence first) :text first-text))
        (when (string-empty-p last-text)
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
  (or (gdocs-convert-status-native-parts element markers)
      (gdocs-convert-footnote-native-parts element markers)
      (gdocs-convert-date-native-parts element markers)
      (gdocs-convert-status-unsupported-native element markers)))

(defun gdocs-convert--inline-finalize-runs (runs)
  "Compose feature-owned completion of reconstructed RUNS."
  (gdocs-convert-status-finalize-runs runs))

(defun gdocs-convert--inline-run-policy (run)
  "Compose feature-owned normalization and addressing policy for RUN."
  (append (gdocs-convert-date-run-policy run)
          (gdocs-convert-status-run-policy run)
          (gdocs-convert-footnote-run-policy run)))

(defun gdocs-convert--inline-link-for-org (link)
  "Resolve LINK through the current document link context, if available."
  (if gdocs-convert--link-context (gdocs-convert--reverse-resolve-link link) link))

(defun gdocs-convert--link-url (link)
  "Extract the URL string from a LINK element.
When `gdocs-convert--link-context' is bound, resolves file: and
id: links to Google Docs URLs for cross-document linking."
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link))
        (search (org-element-property :search-option link)))
    (cond
     ((member type '("http" "https" "ftp" "mailto"))
      (concat type ":" path))
     ((and gdocs-convert--link-context (string= type "file"))
      (gdocs-convert--resolve-file-link path search))
     ((and gdocs-convert--link-context (string= type "id"))
      (gdocs-convert--resolve-id-link path))
     ((string= type "fuzzy")
      ;; Same-document heading link: [[*Heading]] or [[#custom-id]]
      (if (and gdocs-convert--link-context
               (string-prefix-p "*" path))
          (gdocs-convert--resolve-same-doc-heading-link
           (substring path 1))
        path))
     ;; Preserve file: prefix for non-org file links
     ((string= type "file")
      (concat "file:" path))
     ;; Preserve id: prefix for org-id links
     ((string= type "id")
      (concat "id:" path))
     (t path))))

(defun gdocs-convert--resolve-file-link (path search)
  "Resolve a file: link PATH to a Google Docs URL.
SEARCH is the search option from org-element (e.g. \"*Heading\").
If the target is an org file with no Google Doc ID, returns nil so
the link is rendered as plain text in Google Docs.  Non-org file
links are preserved with a file: prefix."
  (let* ((buffer-file (plist-get gdocs-convert--link-context :buffer-file))
         (abs-path (if (file-name-absolute-p path)
                       path
                     (expand-file-name path
                                       (file-name-directory buffer-file))))
         (target-doc-id (gdocs-convert--read-file-property-gdocs-id abs-path))
         ;; Strip * prefix from heading search options
         (heading-text (when (and search (string-prefix-p "*" search))
                         (substring search 1))))
    (cond
     (target-doc-id
      (gdocs-convert--make-docs-url target-doc-id heading-text))
     ;; Org file without a Google Doc: no link (plain text)
     ((string-suffix-p ".org" path t)
      nil)
     (t
      (concat "file:" path)))))

(defun gdocs-convert--resolve-id-link (id)
  "Resolve an id: link ID to a Google Docs URL.
Uses `org-id-find' to locate the target file, then reads its
`gdocs-document-id'.  Returns nil when the target has no Google
Doc, so the link is rendered as plain text.
Suppresses `org-id-update-id-locations' inside `org-id-find' so
that uncached IDs do not trigger a full scan of all org files,
which can freeze Emacs for several seconds."
  (when-let* ((location
               (cl-letf (((symbol-function 'org-id-update-id-locations) #'ignore))
                 (org-id-find id))))
    (let* ((file (if (consp location) (car location) location))
           (target-doc-id (gdocs-convert--read-file-property-gdocs-id file)))
      (when target-doc-id
        (let* ((pos (when (consp location) (cdr location)))
               (heading-text (when pos
                               (gdocs-convert--heading-at-pos file pos))))
          (gdocs-convert--make-docs-url target-doc-id heading-text))))))

(defun gdocs-convert--resolve-same-doc-heading-link (heading-text)
  "Resolve a same-document heading link to HEADING-TEXT.
When the current document's heading cache is populated, returns a
Google Docs URL with heading anchor.  Otherwise returns the path."
  (let* ((buffer-file (plist-get gdocs-convert--link-context :buffer-file))
         (current-doc-id (when buffer-file
                           (gdocs-convert--read-file-property-gdocs-id
                            buffer-file))))
    (if current-doc-id
        (gdocs-convert--make-docs-url current-doc-id heading-text)
      (concat "*" heading-text))))

(defun gdocs-convert--heading-at-pos (file pos)
  "Return the heading text at position POS in FILE, or nil."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char pos)
    (when (org-at-heading-p)
      (org-element-property :raw-value (org-element-at-point)))))

(defun gdocs-convert--make-docs-url (doc-id &optional heading-search)
  "Construct a Google Docs URL for DOC-ID.
If HEADING-SEARCH is non-nil, look up the heading ID in the
heading cache and append a #heading=h.ID fragment."
  (let ((base (format "https://docs.google.com/document/d/%s/edit" doc-id)))
    (if heading-search
        (let* ((cache-entry (gethash doc-id gdocs-convert--heading-cache))
               (normalized (s-trim (downcase heading-search)))
               (heading-id
                (cl-loop for (text . id) in cache-entry
                         when (string= (s-trim (downcase text)) normalized)
                         return id)))
          (if heading-id
              (concat base "#heading=h." heading-id)
            base))
      base)))

;; ---------------------------------------------------------------------------
;;; Lists -> IR

(defun gdocs-convert--org-list-type (plain-list)
  "Determine the IR list type symbol for PLAIN-LIST.
Returns `bullet', `number', or `check'."
  (let ((type (org-element-property :type plain-list)))
    (pcase type
      ('ordered 'number)
      ('descriptive 'bullet)
      (_ (if (gdocs-convert--list-has-checkboxes-p plain-list)
             'check
           'bullet)))))

(defun gdocs-convert--list-has-checkboxes-p (plain-list)
  "Return non-nil if any item in PLAIN-LIST has a checkbox."
  (org-element-map plain-list 'item
    (lambda (item)
      (org-element-property :checkbox item))
    nil t))

(defun gdocs-convert--strip-continuation-indent (runs)
  "Strip continuation line indentation from list item RUNS.
When `org-element' parses a multi-line list item, it preserves
the structural indentation in the text.  This function removes
it so the IR text is clean for round-tripping."
  (mapcar (lambda (run)
            (let ((text (plist-get run :text)))
              (if (string-match-p "\n" text)
                  (plist-put (copy-sequence run) :text
                             (replace-regexp-in-string
                              "\n[ \t]+" "\n" text))
                run)))
          runs))

;; ---------------------------------------------------------------------------
;;; Tables -> IR

(defun gdocs-convert--table-to-ir (table)
  "Convert an org TABLE element to an IR table element."
  (let ((rows (gdocs-convert--table-rows table)))
    (list :type 'table
          :rows rows
          :id (gdocs-convert--next-id))))

(defun gdocs-convert--table-rows (table)
  "Extract rows from an org TABLE as a list of lists of text run lists.
Skips horizontal rule rows."
  (let ((result nil))
    (dolist (row (org-element-contents table))
      (when (and (eq (org-element-type row) 'table-row)
                 (eq (org-element-property :type row) 'standard))
        (push (gdocs-convert--table-row-cells row) result)))
    (nreverse result)))

(defun gdocs-convert--table-row-cells (row)
  "Extract cells from a table ROW as a list of text run lists."
  (let ((cells nil))
    (dolist (cell (org-element-contents row))
      (when (eq (org-element-type cell) 'table-cell)
        (push (gdocs-convert--objects-to-runs
               (org-element-contents cell))
              cells)))
    (nreverse cells)))

;; ---------------------------------------------------------------------------
;;; Quote blocks -> IR

(defun gdocs-convert--quote-block-to-ir (quote-block)
  "Convert a QUOTE-BLOCK element to semantic quote-block IR."
  (let ((paragraphs (gdocs-convert--block-paragraph-runs quote-block)))
    (list :type 'quote-block
          :paragraphs (or paragraphs
                          (list (list (gdocs-convert--make-plain-run ""))))
          :id (gdocs-convert--next-id))))

;; ---------------------------------------------------------------------------
;;; Src blocks / keywords -> IR (org-only markers)

(defun gdocs-convert--block-line-style-name (base-style index count)
  "Return logical block-line style from BASE-STYLE, INDEX, and COUNT.
The naming convention is shared by typographic block renderers:
BASE-STYLE-first, BASE-STYLE-line, BASE-STYLE-last, and BASE-STYLE-single."
  (intern (format "%s-%s"
                  base-style
                  (cond
                   ((= count 1) "single")
                   ((= index 0) "first")
                   ((= index (1- count)) "last")
                   (t "line")))))

(defun gdocs-convert--typographic-block-paragraph
    (element contents style-base marker index count)
  "Return shared paragraph IR for typographic block ELEMENT.
CONTENTS is a run list, STYLE-BASE is the logical style family,
MARKER is a marker plist, and INDEX/COUNT select first/line/last/single
line style names."
  (gdocs-convert-make-paragraph
   contents
   :style 'normal
   :style-name (gdocs-convert--block-line-style-name
                style-base index count)
   :gdocs-marker (list marker)
   :id (plist-get element :id)))

(defun gdocs-convert--src-block-to-ir (src-block)
  "Convert a SRC-BLOCK to a semantic source-block IR element."
  (let* ((language (or (org-element-property :language src-block) ""))
         (value (s-trim-right (or (org-element-property :value src-block) "")))
         (lines (split-string value "\n")))
    (list :type 'source-block
          :language language
          :lines lines
          :id (gdocs-convert--next-id))))

(defun gdocs-convert--lines-to-monospace-ir (lines)
  "Convert LINES to a list of monospace paragraph IR elements.
Each line becomes a separate paragraph with a code run, matching
the Google Docs representation."
  (mapcar (lambda (line)
            (list :type 'paragraph
                  :style 'normal
                  :contents (list (gdocs-convert--make-run
                                  line (gdocs-convert--code-props)))
                  :id (gdocs-convert--next-id)))
          lines))

(defun gdocs-convert--example-block-to-ir (example-block)
  "Convert an EXAMPLE-BLOCK to semantic example-block IR."
  (let* ((value (or (org-element-property :value example-block) ""))
         (lines (split-string (s-trim-right value) "\n")))
    (list :type 'example-block
          :lines lines
          :id (gdocs-convert--next-id))))

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
  (if (gdocs-convert--callout-block-p special-block)
      (list (gdocs-convert--callout-block-to-ir special-block))
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

(defun gdocs-convert--callout-block-p (special-block)
  "Return non-nil if SPECIAL-BLOCK is the canonical callout block."
  (string= (upcase (or (org-element-property :type special-block) ""))
           "CALLOUT"))

(defun gdocs-convert--callout-attributes (special-block)
  "Return callout attributes from SPECIAL-BLOCK."
  (let* ((attr (org-export-read-attribute :attr_callout special-block))
         (type (or (plist-get attr :type) "info"))
         (title (plist-get attr :title)))
    (list :type (gdocs-convert--unquote-org-attribute type)
          :title (gdocs-convert--unquote-org-attribute title))))

(defun gdocs-convert--unquote-org-attribute (value)
  "Return VALUE without one layer of Org attribute string quoting."
  (cond
   ((not (stringp value)) value)
   ((string-match-p "\\`\".*\"\\'" value)
    (condition-case nil
        (car (read-from-string value))
      (error (string-trim value "\"" "\""))))
   (t value)))

(defun gdocs-convert--validate-callout-type (type)
  "Signal an error unless TYPE is a supported callout type."
  (unless (member type gdocs-callout-types)
    (user-error "Unsupported Org callout type `%s'; supported types: %s"
                type (s-join ", " gdocs-callout-types))))

(defun gdocs-convert--validate-callout-body (special-block)
  "Signal when SPECIAL-BLOCK callout contains unsupported direct children."
  (gdocs-convert--block-body-elements special-block '(paragraph plain-list)
                                      "callout")
  nil)

(defun gdocs-convert--callout-body-elements (special-block)
  "Return ordered paragraph IR elements for SPECIAL-BLOCK callout body."
  (gdocs-convert--block-body-elements special-block '(paragraph plain-list)
                                      "callout"))

(defun gdocs-convert--callout-block-to-ir (special-block)
  "Convert canonical Org SPECIAL-BLOCK callout to callout-block IR."
  (let* ((attributes (gdocs-convert--callout-attributes special-block))
         (type (or (plist-get attributes :type) "info"))
         (title (plist-get attributes :title))
         (body-elements (gdocs-convert--callout-body-elements special-block)))
    (gdocs-convert--validate-callout-type type)
    (list :type 'callout-block
          :callout-type type
          :title title
          :body-elements body-elements
          :paragraphs (mapcar (lambda (element)
                                (plist-get element :contents))
                              body-elements)
          :id (gdocs-convert--next-id))))

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
        (len (length (plist-get split :body)))
        (list-counter 0)
        (prev-list-level nil))
    (while (< i len)
      (let ((element (aref elements i)))
        (cond
         ;; Empty paragraph (nil contents): skip in org output but
         ;; preserve in IR for correct index computation.
         ((and (eq (plist-get element :type) 'paragraph)
               (null (plist-get element :contents)))
          (setq i (1+ i)))
         ;; Collect consecutive all-code paragraphs into an example block
         ((gdocs-convert--all-code-paragraph-p element)
          (let ((code-lines nil))
            (while (and (< i len)
                        (gdocs-convert--all-code-paragraph-p
                         (aref elements i)))
              (push (gdocs-convert--plain-text (aref elements i))
                    code-lines)
              (setq i (1+ i)))
            (when (and parts prev-type)
              (push "" parts))
            (push (format "#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE"
                          (s-join "\n" (nreverse code-lines)))
                  parts)
            (setq prev-element (list :type 'example-block)
                  prev-type 'example-block)
            (setq list-counter 0 prev-list-level nil)))
         ;; Normal element
         (t
          (let* ((type (plist-get element :type))
                 (list-info (plist-get element :list))
                 (list-level (when list-info (plist-get list-info :level)))
                 (list-type (when list-info (plist-get list-info :type))))
            ;; Track sequential numbering for ordered list items
            (if (eq list-type 'number)
                (if (eql list-level prev-list-level)
                    (cl-incf list-counter)
                  (setq list-counter 1))
              (setq list-counter 0))
            (setq prev-list-level (when (eq list-type 'number) list-level))
            ;; Mutates the :list plist in place — also done by
            ;; `gdocs-convert--assign-list-counters' for the docs->IR path.
            (when (and list-info (> list-counter 0))
              (plist-put list-info :counter list-counter))
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

(defun gdocs-convert--src-block-data (element)
  "Return source-block marker data from ELEMENT, or nil."
  (gdocs-convert--extract-marker-field
   (plist-get element :gdocs-marker) 'src-block))

(defun gdocs-convert--example-block-data (element)
  "Return example-block marker data from ELEMENT, or nil."
  (gdocs-convert--extract-marker-field
   (plist-get element :gdocs-marker) 'example-block))

(defun gdocs-convert--src-block-paragraph-p (element)
  "Return non-nil if ELEMENT is a marked source-block paragraph."
  (and (eq (plist-get element :type) 'paragraph)
       (gdocs-convert--src-block-data element)))

(defun gdocs-convert--example-block-paragraph-p (element)
  "Return non-nil if ELEMENT is a marked example-block paragraph."
  (and (eq (plist-get element :type) 'paragraph)
       (gdocs-convert--example-block-data element)))

(defun gdocs-convert--quote-block-marker (element)
  "Return the quote-block marker on ELEMENT, or nil."
  (seq-find (lambda (marker)
              (eq (plist-get marker :type) 'quote-block))
            (plist-get element :gdocs-marker)))

(defun gdocs-convert--quote-block-paragraph-p (element)
  "Return non-nil if ELEMENT is a marked quote-block paragraph."
  (and (eq (plist-get element :type) 'paragraph)
       (gdocs-convert--quote-block-marker element)))

(defun gdocs-convert--all-code-paragraph-p (element)
  "Return non-nil if ELEMENT is a normal paragraph with all-code runs.
Paragraphs that carry any marker are excluded, since they
already round-trip via named ranges."
  (and (eq (plist-get element :type) 'paragraph)
       (eq (plist-get element :style) 'normal)
       (not (plist-get element :list))
       (not (plist-get element :gdocs-marker))
       (not (plist-get element :alignment))
       (let ((runs (plist-get element :contents)))
         (and runs (seq-every-p (lambda (r) (plist-get r :code)) runs)))))

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
          (list-info (plist-get element :list))
          (block-types '(source-block example-block quote-block callout-block)))
      (or (memq previous-type block-types)
          (memq type block-types)
          (and list-info (not (plist-get previous :list)))
          (and (eq previous-type 'paragraph)
               (gdocs-convert--heading-style-p previous-style))
          (eq type 'table)
          (eq type 'horizontal-rule)
          (eq type 'footnote)
          (eq type 'image)
          (and (eq type 'paragraph)
               (not list-info)
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
  (or (gdocs-convert-status-render-run run)
      (gdocs-convert-footnote-run-to-org run)
      (gdocs-convert-inline-plain-run-to-org run)))

(defun gdocs-convert--reverse-resolve-link (url)
  "Reverse-resolve a Google Docs URL to an org link.
If URL matches a known Google Doc, returns an org link.  For
same-document heading links, returns *Heading.  For
cross-document links, returns file:relative-path::*Heading.
Falls back to URL unchanged for unknown documents."
  (if (string-match gdocs-convert--docs-url-regexp url)
      (let* ((doc-id (match-string 1 url))
             (heading-id (match-string 2 url))
             (docid-map (plist-get gdocs-convert--link-context :docid-map))
             (buffer-file (plist-get gdocs-convert--link-context :buffer-file))
             (target-file (when docid-map (gethash doc-id docid-map))))
        (if target-file
            (let ((heading-text (gdocs-convert--lookup-heading-text
                                 doc-id heading-id))
                  (same-doc-p (gdocs-convert--same-file-p
                               target-file buffer-file)))
              (gdocs-convert--format-org-link
               target-file buffer-file heading-text same-doc-p))
          url))
    url))

(defun gdocs-convert--lookup-heading-text (doc-id heading-id)
  "Look up heading text for HEADING-ID in DOC-ID's cache.
Returns the heading text string or nil."
  (when heading-id
    (let ((reverse-alist (gethash doc-id
                                  gdocs-convert--heading-reverse-cache)))
      (cdr (assoc heading-id reverse-alist)))))

(defun gdocs-convert--same-file-p (file-a file-b)
  "Return non-nil if FILE-A and FILE-B refer to the same file."
  (and file-a file-b
       (string= (expand-file-name file-a)
                (expand-file-name file-b))))

(defun gdocs-convert--format-org-link (target-file buffer-file
                                                    heading-text same-doc-p)
  "Format an org link to TARGET-FILE from BUFFER-FILE.
HEADING-TEXT is the heading anchor, or nil.  SAME-DOC-P
indicates a link within the same document."
  (cond
   ((and same-doc-p heading-text)
    (gdocs-convert--same-doc-heading-link buffer-file heading-text))
   (heading-text
    (format "file:%s::*%s"
            (file-relative-name target-file
                                (file-name-directory buffer-file))
            heading-text))
   (t
    (concat "file:"
            (file-relative-name target-file
                                (file-name-directory buffer-file))))))

(defun gdocs-convert--same-doc-heading-link (file heading-text)
  "Format a same-document link to HEADING-TEXT in FILE.
Produces id:UUID when the heading has an org ID, preserving
robust links that survive heading renames.  Falls back to
*Heading when no org ID is found."
  (if-let* ((org-id (gdocs-convert--find-heading-org-id
                      file heading-text)))
      (concat "id:" org-id)
    (concat "*" heading-text)))

(defun gdocs-convert--find-heading-org-id (file heading-text)
  "Find the org ID for the heading matching HEADING-TEXT in FILE.
Returns the UUID string or nil."
  (when (and file heading-text (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (delay-mode-hooks (org-mode))
      (goto-char (point-min))
      (let ((normalized (s-trim (downcase heading-text))))
        (catch 'found
          (while (re-search-forward org-heading-regexp nil t)
            (when (string= (s-trim (downcase (org-get-heading t t t t)))
                           normalized)
              (throw 'found (org-entry-get nil "ID")))))))))

;; ---------------------------------------------------------------------------
;;; IR footnote -> org

;; ---------------------------------------------------------------------------
;;; IR image -> org

(defun gdocs-convert--ir-image-to-org (element)
  "Convert an IR image ELEMENT to an Org image link."
  (let ((target (or (plist-get element :path)
                    (plist-get element :source-uri)
                    (plist-get element :content-uri)
                    (plist-get element :object-id)
                    ""))
        (caption (plist-get element :caption)))
    (concat (when (and caption (not (string-empty-p caption)))
              (format "#+CAPTION: %s\n" caption))
            (format "[[%s]]" (if (string-prefix-p "file:" target)
                                  target
                                (if (plist-get element :path)
                                    (concat "file:" target)
                                  target))))))

(defun gdocs-convert--ir-source-block-to-org (element)
  "Convert source-block IR ELEMENT to Org source block text."
  (gdocs-convert--format-src-block-from-lines
   (plist-get element :language)
   (plist-get element :lines)))

(defun gdocs-convert--ir-example-block-to-org (element)
  "Convert example-block IR ELEMENT to Org example block text."
  (format "#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE"
          (s-join "\n" (or (plist-get element :lines) '("")))))

(defun gdocs-convert--ir-quote-block-to-org (element)
  "Convert quote-block IR ELEMENT to Org quote block text."
  (format "#+BEGIN_QUOTE\n%s\n#+END_QUOTE"
          (s-join "\n\n"
                  (mapcar #'gdocs-convert--runs-to-org
                          (plist-get element :paragraphs)))))

(defun gdocs-convert--callout-body-elements-from-paragraphs (element)
  "Return backward-compatible callout body elements for ELEMENT."
  (or (plist-get element :body-elements)
      (mapcar (lambda (runs)
                (list :type 'paragraph
                      :style 'normal
                      :contents runs))
              (plist-get element :paragraphs))))

(defun gdocs-convert--join-callout-body-org (elements)
  "Join callout body ELEMENTS as Org text."
  (gdocs-convert--assign-list-counters elements)
  (let ((parts nil)
        previous-list-p)
    (dolist (element elements)
      (let ((current-list-p (plist-get element :list))
            (text (gdocs-convert--ir-paragraph-to-org element)))
        (when parts
          (push (if (or previous-list-p current-list-p) "\n" "\n\n") parts))
        (push text parts)
        (setq previous-list-p current-list-p)))
    (apply #'concat (nreverse parts))))

(defun gdocs-convert--ir-callout-block-to-org (element)
  "Convert callout-block IR ELEMENT to Org callout block text."
  (let* ((type (or (plist-get element :callout-type) "info"))
         (title (plist-get element :title))
         (attrs (concat "#+ATTR_CALLOUT: :type " type
                        (if (and title (not (string-empty-p title)))
                            (format " :title %S" title)
                          "")))
         (body (gdocs-convert--join-callout-body-org
                (gdocs-convert--callout-body-elements-from-paragraphs element))))
    (format "%s\n#+BEGIN_CALLOUT\n%s\n#+END_CALLOUT" attrs body)))

;; ---------------------------------------------------------------------------
;;; IR table -> org

(defun gdocs-convert--ir-table-to-org (element)
  "Convert an IR table ELEMENT to an org table string."
  (let* ((rows (plist-get element :rows))
         (formatted-rows (mapcar #'gdocs-convert--format-table-row rows))
         (parts nil))
    (when formatted-rows
      (push (car formatted-rows) parts)
      (when (cdr formatted-rows)
        (push (gdocs-convert--table-separator (car rows)) parts)
        (dolist (row (cdr formatted-rows))
          (push row parts))))
    (s-join "\n" (nreverse parts))))

(defun gdocs-convert--format-table-row (row)
  "Format a table ROW (list of cell run lists) as an org table row string."
  (let ((cells (mapcar (lambda (cell-runs)
                         (gdocs-convert--runs-to-org cell-runs))
                       row)))
    (concat "| " (s-join " | " cells) " |")))

(defun gdocs-convert--table-separator (row)
  "Generate a table separator string matching the column count of ROW."
  (let ((cols (length row)))
    (concat "|" (s-join "+" (make-list cols "---")) "|")))

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
                   (append (nreverse result)
                           (gdocs-convert--docs-footnotes-to-ir
                            (alist-get 'footnotes json) footnote-refs)))))))))
      (gdocs-convert--assign-list-counters ir)
      ir)))

(defun gdocs-convert--image-caption-marker-p (marker)
  "Return non-nil when MARKER marks an image caption."
  (eq (plist-get marker :type) 'image-caption))

(defun gdocs-convert--image-caption-paragraph-p (element)
  "Return non-nil when ELEMENT is a marked image caption paragraph."
  (and (eq (plist-get element :type) 'paragraph)
       (seq-some #'gdocs-convert--image-caption-marker-p
                 (plist-get element :gdocs-marker))))

(defun gdocs-convert--attach-image-caption-markers (ir)
  "Attach marked image caption paragraphs in IR to preceding image elements."
  (let ((result nil))
    (dolist (element ir)
      (if (and (gdocs-convert--image-caption-paragraph-p element)
               result
               (eq (plist-get (car result) :type) 'image))
          (let ((image (copy-sequence (car result)))
                (caption (gdocs-convert--runs-to-plain-text
                          (plist-get element :contents))))
            (plist-put image :caption caption)
            (when-let* ((caption-end (plist-get element :doc-end)))
              (plist-put image :doc-end caption-end))
            (setq result (cons image (cdr result))))
        (push element result)))
    (nreverse result)))

(defun gdocs-convert--callout-marker-p (marker)
  "Return non-nil if MARKER is a callout grouping marker."
  (memq (plist-get marker :type) '(callout-label callout-body)))

(defun gdocs-convert--callout-marker (element)
  "Return the first callout marker on ELEMENT, or nil."
  (seq-find #'gdocs-convert--callout-marker-p
            (plist-get element :gdocs-marker)))

(defun gdocs-convert--callout-marker-data (marker)
  "Return plist data decoded from callout MARKER."
  (let* ((raw (or (plist-get marker :data) "info:"))
         (parts (s-split ":" raw))
         (type (or (car parts) "info"))
         (title (when-let* ((encoded (cadr parts)))
                  (url-unhex-string encoded))))
    (list :type type :title (and title (not (string-empty-p title)) title))))

(defun gdocs-convert--callout-list-marker (element)
  "Return the callout list marker on ELEMENT, or nil."
  (seq-find (lambda (marker)
              (eq (plist-get marker :type) 'callout-list))
            (plist-get element :gdocs-marker)))

(defun gdocs-convert--callout-list-marker-data (marker)
  "Return list metadata decoded from callout-list MARKER."
  (let* ((raw (or (plist-get marker :data) "bullet:0"))
         (parts (s-split ":" raw))
         (type (intern (or (car parts) "bullet")))
         (level (string-to-number (or (cadr parts) "0"))))
    (list :type type :level level)))

(defun gdocs-convert--strip-callout-list-prefix (element list-info)
  "Return ELEMENT contents with visible LIST-INFO marker prefix removed."
  (let* ((marker (gdocs-convert--list-marker
                  (plist-get list-info :type) list-info))
         (copy (copy-sequence element))
         (runs (copy-sequence (plist-get copy :contents))))
    (when runs
      (let ((first (copy-sequence (car runs))))
        (when (string-prefix-p marker (or (plist-get first :text) ""))
          (plist-put first :text (substring (plist-get first :text)
                                            (length marker)))
          (setq runs (cons first (cdr runs)))
          (plist-put copy :contents runs))))
    copy))

(defun gdocs-convert--restore-callout-list-element (element)
  "Return ELEMENT with callout-list marker restored as list metadata."
  (if-let* ((marker (gdocs-convert--callout-list-marker element)))
      (let* ((list-info (gdocs-convert--callout-list-marker-data marker))
             (copy (gdocs-convert--strip-callout-list-prefix element
                                                             list-info)))
        (plist-put copy :list list-info)
        copy)
    element))

(defun gdocs-convert--empty-paragraph-p (element)
  "Return non-nil when ELEMENT is an empty paragraph."
  (and (eq (plist-get element :type) 'paragraph)
       (let ((contents (plist-get element :contents)))
         (or (null contents)
             (and (= (length contents) 1)
                  (string-empty-p
                   (string-trim
                    (or (plist-get (car contents) :text) ""))))))))

(defun gdocs-convert--consume-generated-separator (remaining end)
  "Consume one generated empty paragraph from REMAINING.
Return a plist with :remaining and :end.  Typographic block renderers use an
unmarked empty paragraph as separator chrome; pull grouping treats it as owned
by the preceding marked block so repeated push/restyle operations stay
idempotent."
  (if (and remaining (gdocs-convert--empty-paragraph-p (car remaining)))
      (list :remaining (cdr remaining)
            :end (or (plist-get (car remaining) :doc-end) end))
    (list :remaining remaining :end end)))

(defun gdocs-convert--attach-callout-block-markers (ir)
  "Group marked callout paragraphs in IR.
Consume one following empty paragraph as generated callout separator chrome so
subsequent diffs treat the separator as part of the owning callout block."
  (let ((result nil)
        (remaining ir))
    (while remaining
      (let* ((element (car remaining))
             (marker (gdocs-convert--callout-marker element)))
        (if marker
            (let* ((data (gdocs-convert--callout-marker-data marker))
                   (type (plist-get data :type))
                   (title (plist-get data :title))
                   (paragraphs nil)
                   (last-body-marker-id nil)
                   (start (plist-get element :doc-start))
                   end)
              (while (and remaining
                          (let ((candidate-marker
                                 (gdocs-convert--callout-marker (car remaining))))
                            (and candidate-marker
                                 (equal data
                                        (gdocs-convert--callout-marker-data
                                         candidate-marker)))))
                (when (and (eq (plist-get (gdocs-convert--callout-marker
                                           (car remaining))
                                          :type)
                               'callout-body)
                           (not (gdocs-convert--empty-paragraph-p
                                 (car remaining))))
                  (let* ((marker (gdocs-convert--callout-marker (car remaining)))
                         (marker-id (plist-get marker :id))
                         (restored (gdocs-convert--restore-callout-list-element
                                    (car remaining))))
                    (if (and paragraphs
                             marker-id
                             (equal marker-id last-body-marker-id)
                             (not (plist-get restored :list))
                             (not (plist-get (car paragraphs) :list))
                             (gdocs-convert--soft-wrap-continuation-p
                              (plist-get restored :contents)))
                        (let ((merged (copy-sequence (car paragraphs))))
                          (plist-put
                           merged :contents
                           (gdocs-convert--merge-adjacent-runs
                            (append (plist-get (car paragraphs) :contents)
                                    (list (gdocs-convert--make-plain-run " "))
                                    (plist-get restored :contents))))
                          (setcar paragraphs merged))
                      (push restored paragraphs))
                    (setq last-body-marker-id marker-id)))
                (setq end (plist-get (car remaining) :doc-end))
                (setq remaining (cdr remaining)))
              (let ((separator (gdocs-convert--consume-generated-separator
                                remaining end)))
                (setq remaining (plist-get separator :remaining))
                (setq end (plist-get separator :end)))
              (let ((body-elements (nreverse paragraphs)))
                (push (append (list :type 'callout-block
                                    :callout-type type
                                    :title title
                                    :body-elements body-elements
                                    :paragraphs (mapcar (lambda (paragraph)
                                                          (plist-get paragraph
                                                                     :contents))
                                                        body-elements)
                                    :id (gdocs-convert--next-id))
                            (when start (list :doc-start start))
                            (when end (list :doc-end end)))
                    result)))
          (push element result)
          (setq remaining (cdr remaining)))))
    (nreverse result)))

(defun gdocs-convert--strip-quote-visual-style (runs)
  "Return RUNS without visual-only quote paragraph styling."
  (mapcar (lambda (run)
            (plist-put (copy-sequence run) :italic nil))
          runs))

(defun gdocs-convert--soft-wrap-continuation-p (runs)
  "Return non-nil when RUNS look like a split soft-wrapped paragraph line."
  (let ((text (string-trim-left (gdocs-convert--runs-to-plain-text runs))))
    (and (not (string-empty-p text))
         (let ((first (aref text 0)))
           (or (and (>= first ?a) (<= first ?z))
               (member first '(?, ?. ?\; ?: ?\) ?\] ?\})))))))

(defun gdocs-convert--attach-quote-block-markers (ir)
  "Group consecutive marked quote-block paragraphs in IR."
  (let ((result nil)
        (remaining ir))
    (while remaining
      (let ((element (car remaining)))
        (if (gdocs-convert--quote-block-paragraph-p element)
            (let ((paragraphs nil)
                  (last-marker-id nil)
                  (start (plist-get element :doc-start))
                  end)
              (while (and remaining
                          (gdocs-convert--quote-block-paragraph-p
                           (car remaining)))
                (unless (gdocs-convert--empty-paragraph-p (car remaining))
                  (let* ((marker (gdocs-convert--quote-block-marker
                                  (car remaining)))
                         (marker-id (plist-get marker :id))
                         (runs (gdocs-convert--strip-quote-visual-style
                                (plist-get (car remaining) :contents))))
                    (if (and paragraphs
                             marker-id
                             (equal marker-id last-marker-id)
                             (gdocs-convert--soft-wrap-continuation-p runs))
                        (setcar paragraphs
                                (gdocs-convert--merge-adjacent-runs
                                 (append (car paragraphs)
                                         (list (gdocs-convert--make-plain-run
                                                " "))
                                         runs)))
                      (push runs paragraphs))
                    (setq last-marker-id marker-id)))
                (setq end (plist-get (car remaining) :doc-end))
                (setq remaining (cdr remaining)))
              (let ((separator (gdocs-convert--consume-generated-separator
                                remaining end)))
                (setq remaining (plist-get separator :remaining))
                (setq end (plist-get separator :end)))
              (push (append (list :type 'quote-block
                                  :paragraphs (nreverse paragraphs)
                                  :id (gdocs-convert--next-id))
                            (when start (list :doc-start start))
                            (when end (list :doc-end end)))
                    result))
          (push element result)
          (setq remaining (cdr remaining)))))
    (nreverse result)))

(defun gdocs-convert--attach-example-block-markers (ir)
  "Group consecutive marked example-block paragraphs in IR."
  (let ((result nil)
        (remaining ir))
    (while remaining
      (let ((element (car remaining)))
        (if (gdocs-convert--example-block-paragraph-p element)
            (let ((block-id (gdocs-convert--example-block-data element))
                  (lines nil)
                  (start (plist-get element :doc-start))
                  end)
              (while (and remaining
                          (gdocs-convert--example-block-paragraph-p
                           (car remaining))
                          (equal block-id
                                 (gdocs-convert--example-block-data
                                  (car remaining))))
                (push (gdocs-convert--plain-text (car remaining)) lines)
                (setq end (plist-get (car remaining) :doc-end))
                (setq remaining (cdr remaining)))
              (push (append (list :type 'example-block
                                  :lines (nreverse lines)
                                  :id (gdocs-convert--next-id))
                            (when start (list :doc-start start))
                            (when end (list :doc-end end)))
                    result))
          (push element result)
          (setq remaining (cdr remaining)))))
    (nreverse result)))

(defun gdocs-convert--attach-source-block-markers (ir)
  "Group consecutive marked source-block paragraphs in IR."
  (let ((result nil)
        (remaining ir))
    (while remaining
      (let ((element (car remaining)))
        (if (gdocs-convert--src-block-paragraph-p element)
            (let ((language (gdocs-convert--src-block-data element))
                  (lines nil)
                  (start (plist-get element :doc-start))
                  end)
              (while (and remaining
                          (gdocs-convert--src-block-paragraph-p (car remaining))
                          (equal language
                                 (gdocs-convert--src-block-data (car remaining))))
                (push (gdocs-convert--plain-text (car remaining)) lines)
                (setq end (plist-get (car remaining) :doc-end))
                (setq remaining (cdr remaining)))
              (push (append (list :type 'source-block
                                  :language language
                                  :lines (nreverse lines)
                                  :id (gdocs-convert--next-id))
                            (when start (list :doc-start start))
                            (when end (list :doc-end end)))
                    result))
          (push element result)
          (setq remaining (cdr remaining)))))
    (nreverse result)))

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
    (when (seq-some (lambda (marker)
                      (and (eq (plist-get marker :type) 'status)
                           (< (plist-get marker :start) (plist-get marker :end))))
                    (gdocs-convert--markers-for-range markers
                                                      (alist-get 'startIndex element)
                                                      (alist-get 'endIndex element)))
      (user-error "Status labels inside Google Docs tables are not supported"))
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
        (when (alist-get 'inlineObjectElement el)
          (let ((obj-id (alist-get 'inlineObjectId
                                   (alist-get 'inlineObjectElement el))))
            (throw 'found
                   (gdocs-convert--docs-inline-object-to-image-ir
                    obj-id inline-objects
                    (gdocs-convert--markers-for-range markers start end))))))
      nil)))

(defun gdocs-convert--image-source-marker (markers)
  "Return image source marker from MARKERS, or nil."
  (seq-find (lambda (marker)
              (eq (plist-get marker :type) 'image-source))
            markers))

(defun gdocs-convert--docs-inline-object-to-image-ir
    (object-id inline-objects &optional markers)
  "Return image IR for inline image OBJECT-ID using INLINE-OBJECTS metadata.
MARKERS may contain original Org source ownership metadata."
  (let* ((inline-object (and object-id
                             (alist-get (intern object-id) inline-objects)))
         (properties (alist-get 'inlineObjectProperties inline-object))
         (embedded-object (alist-get 'embeddedObject properties))
         (image-properties (alist-get 'imageProperties embedded-object))
         (content-uri (alist-get 'contentUri image-properties))
         (source-uri (alist-get 'sourceUri image-properties))
         (source-marker (gdocs-convert--image-source-marker markers))
         (original-path (and source-marker
                             (url-unhex-string
                              (or (plist-get source-marker :data) ""))))
         (title (alist-get 'title embedded-object))
         (description (alist-get 'description embedded-object)))
    (append (list :type 'image
                  :object-id object-id
                  :id (gdocs-convert--next-id))
            (when original-path (list :path original-path))
            (when content-uri (list :content-uri content-uri))
            (when source-uri (list :source-uri source-uri))
            (when title (list :title title))
            (when description (list :description description)))))


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
    (or (gdocs-convert-list-paragraph-from-docs paragraph lists-map markers runs start end)
        (gdocs-convert-heading-paragraph-from-docs paragraph runs markers start end)
        (gdocs-convert-paragraph-from-docs
         paragraph runs markers start end 'normal '(:style-name gdocs-body)))))

(defun gdocs-convert--paragraph-markers-for-range (markers start end)
  "Compose paragraph annotations from MARKERS overlapping START..END."
  (gdocs-convert--without-inline-status-markers
   (gdocs-convert--markers-for-range markers start end)))

(defun gdocs-convert--extract-link-url (link-obj)
  "Extract a URL from a Google Docs LINK-OBJ.
Returns the explicit URL when present.  For internal heading
links (where the API returns a headingId instead of a URL),
constructs a full Google Docs URL using
`gdocs-convert--document-id'."
  (or (alist-get 'url link-obj)
      (gdocs-convert--heading-id-to-url
       (alist-get 'headingId link-obj))))

(defun gdocs-convert--heading-id-to-url (heading-id)
  "Convert internal HEADING-ID to a full Google Docs URL.
Requires `gdocs-convert--document-id' to be bound."
  (when (and heading-id gdocs-convert--document-id)
    (format "https://docs.google.com/document/d/%s/edit#heading=h.%s"
            gdocs-convert--document-id heading-id)))

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
                              (when status-p (list :range-id (alist-get 'namedRangeId range-entry))))
                      result)))))))
    result))

;; ---------------------------------------------------------------------------
;;; Google Docs tables -> IR

(defun gdocs-convert--docs-table-to-ir (table)
  "Convert a Google Docs TABLE to an IR table element."
  (let* ((table-rows (alist-get 'tableRows table))
         (rows (seq-map #'gdocs-convert--docs-table-row-to-ir table-rows)))
    (list :type 'table
          :rows (seq-into rows 'list)
          :id (gdocs-convert--next-id))))

(defun gdocs-convert--docs-table-row-to-ir (row)
  "Convert a Google Docs table ROW to a list of cell run lists."
  (let ((cells (alist-get 'tableCells row)))
    (seq-into (seq-map #'gdocs-convert--docs-table-cell-to-runs cells)
              'list)))

(defun gdocs-convert--docs-table-cell-to-runs (cell)
  "Convert a Google Docs table CELL to a list of text runs."
  (let* ((content (alist-get 'content cell))
         (runs nil))
    (seq-doseq (element content)
      (let ((paragraph (alist-get 'paragraph element)))
        (when paragraph
          (let* ((elements (alist-get 'elements paragraph))
                 (cell-runs (gdocs-convert--docs-elements-to-runs elements))
                 (trimmed (gdocs-convert--trim-trailing-newline-runs
                           cell-runs)))
            (setq runs (nconc runs trimmed))))))
    runs))

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
      (let ((result (gdocs-convert--ir-element-to-requests element index)))
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

(defun gdocs-convert--append-generated-separator-requests
    (requests current element)
  "Append shared generated separator chrome for typographic block ELEMENT.
Return a plist with :requests and :index.  The separator is intentionally
unmarked and unstyled; pull grouping consumes one following empty paragraph as
owned chrome via `gdocs-convert--consume-generated-separator'."
  (let* ((separator (list :type 'paragraph
                          :style 'normal
                          :contents (list (gdocs-convert--make-plain-run ""))
                          :id (plist-get element :id)))
         (result (gdocs-convert--paragraph-to-requests separator current)))
    (list :requests (nconc requests (plist-get result :requests))
          :index (plist-get result :index))))

(defun gdocs-convert--quote-block-paragraph-element (element runs index count)
  "Return paragraph IR for quote-block ELEMENT RUNS at INDEX of COUNT."
  (gdocs-convert--typographic-block-paragraph
   element runs 'gdocs-quote-block (list :type 'quote-block) index count))

(defun gdocs-convert--quote-block-to-requests (element index)
  "Create paragraph requests for quote-block ELEMENT at INDEX."
  (let* ((paragraphs (or (plist-get element :paragraphs)
                         (list (list (gdocs-convert--make-plain-run "")))))
         (count (length paragraphs))
         (paragraph-index 0)
         (requests nil)
         (current index))
    (dolist (runs paragraphs)
      (let* ((paragraph (gdocs-convert--quote-block-paragraph-element
                         element runs paragraph-index count))
             (result (gdocs-convert--paragraph-to-requests paragraph current)))
        (setq requests (nconc requests (plist-get result :requests)))
        (setq current (plist-get result :index))
        (cl-incf paragraph-index)))
    ;; Add unmarked spacing chrome so shaded quote blocks do not visually merge
    ;; with following shaded typographic blocks such as callouts.
    (let ((result (gdocs-convert--append-generated-separator-requests
                   requests current element)))
      (setq requests (plist-get result :requests))
      (setq current (plist-get result :index)))
    (list :requests requests :index current)))

(defun gdocs-convert--callout-style-base (type)
  "Return logical style base symbol for callout TYPE."
  (intern (format "gdocs-callout-%s" type)))

(defun gdocs-convert--callout-label-style (type)
  "Return logical label style symbol for callout TYPE."
  (intern (format "gdocs-callout-%s-label" type)))

(defun gdocs-convert--callout-label (type title)
  "Return visible callout label for TYPE and TITLE."
  (let ((label (or (cdr (assoc type gdocs-callout-labels))
                   (capitalize type))))
    (if (and title (not (string-empty-p title)))
        (format "%s — %s" label title)
      label)))

(defun gdocs-convert--callout-marker-data-string (element)
  "Return encoded callout marker data for ELEMENT."
  (format "%s:%s"
          (or (plist-get element :callout-type) "info")
          (url-hexify-string (or (plist-get element :title) ""))))

(defun gdocs-convert--callout-list-marker-data-string (paragraph)
  "Return encoded callout list marker data for PARAGRAPH."
  (let* ((list-info (plist-get paragraph :list))
         (type (or (plist-get list-info :type) 'bullet))
         (level (or (plist-get list-info :level) 0)))
    (format "%s:%s" type level)))

(defun gdocs-convert--prefix-runs (prefix runs)
  "Return RUNS with PREFIX prepended to the first run."
  (if runs
      (let ((first (copy-sequence (car runs))))
        (plist-put first :text (concat prefix (plist-get first :text)))
        (cons first (cdr runs)))
    (list (gdocs-convert--make-plain-run prefix))))

(defun gdocs-convert--callout-render-list-paragraph (paragraph)
  "Return PARAGRAPH rendered as a native Docs list paragraph."
  (copy-sequence paragraph))

(defun gdocs-convert--callout-paragraph-element (element role body index count)
  "Return paragraph IR for callout ELEMENT ROLE BODY at INDEX of COUNT."
  (let* ((type (or (plist-get element :callout-type) "info"))
         (style-name (if (eq role 'label)
                         (gdocs-convert--callout-label-style type)
                       (gdocs-convert--block-line-style-name
                        (gdocs-convert--callout-style-base type)
                        index count)))
         (source-paragraph (if (and (listp body) (plist-get body :type))
                               (copy-sequence body)
                             (gdocs-convert-make-paragraph
                              body :style 'normal)))
         (list-info (plist-get source-paragraph :list))
         (paragraph (if (and (eq role 'body) list-info)
                        (gdocs-convert--callout-render-list-paragraph
                         source-paragraph)
                      source-paragraph))
         (markers (list (list :type (if (eq role 'label)
                                        'callout-label
                                      'callout-body)
                              :data (gdocs-convert--callout-marker-data-string
                                     element)))))
    (when (and (eq role 'body) list-info)
      (push (list :type 'callout-list
                  :data (gdocs-convert--callout-list-marker-data-string
                         source-paragraph))
            markers))
    (plist-put paragraph :style 'normal)
    (plist-put paragraph :style-name style-name)
    (plist-put paragraph :gdocs-marker (nreverse markers))
    (plist-put paragraph :id (plist-get element :id))
    paragraph))

(defun gdocs-convert--callout-list-indent-reset-request (start end)
  "Return a post-bullet indent reset request from START to END.
Google Docs applies default bullet indentation when `createParagraphBullets'
runs.  Callout list items need native list semantics while keeping the shaded
paragraph box aligned with the rest of the callout, so reset the paragraph
indent after bullet creation."
  `((updateParagraphStyle
     . ((paragraphStyle . ((indentStart . ((magnitude . 0) (unit . "PT")))
                           (indentFirstLine . ((magnitude . 0) (unit . "PT")))))
        (range . ((startIndex . ,start)
                  (endIndex . ,(1- end))))
        (fields . "indentStart,indentFirstLine")))))

(defun gdocs-convert--callout-block-to-requests (element index)
  "Create paragraph requests for callout-block ELEMENT at INDEX."
  (let* ((type (or (plist-get element :callout-type) "info"))
         (title (plist-get element :title))
         (body-elements (gdocs-convert--callout-body-elements-from-paragraphs
                         element))
         (body-count (length body-elements))
         (body-index 0)
         (requests nil)
         (body-ranges nil)
         (current index)
         (label-paragraph
          (gdocs-convert--callout-paragraph-element
           element 'label
           (list (gdocs-convert--make-plain-run
                  (gdocs-convert--callout-label type title)))
           0 1))
         (label-result (gdocs-convert--paragraph-to-requests
                        label-paragraph current)))
    (setq requests (nconc requests (plist-get label-result :requests)))
    (setq current (plist-get label-result :index))
    (dolist (body-element body-elements)
      (let* ((paragraph (gdocs-convert--callout-paragraph-element
                         element 'body body-element body-index body-count))
             (start current)
             (result (gdocs-convert--paragraph-to-requests paragraph current)))
        (push (list :element paragraph
                    :start start
                    :end (plist-get result :index))
              body-ranges)
        (setq requests (nconc requests (plist-get result :requests)))
        (setq current (plist-get result :index))
        (cl-incf body-index)))
    ;; Add unmarked spacing chrome so shaded callouts do not visually merge with
    ;; adjacent shaded typographic blocks.  Empty paragraphs are skipped by Org
    ;; output on pull.  This must be inserted before list fixup because
    ;; `createParagraphBullets' removes leading tabs used for native nesting.
    (let ((result (gdocs-convert--append-generated-separator-requests
                   requests current element)))
      (setq requests (plist-get result :requests))
      (setq current (plist-get result :index)))
    (setq body-ranges (nreverse body-ranges))
    (setq requests (gdocs-convert--fixup-list-nesting requests body-ranges))
    (let ((removed-tabs (apply #'+
                               (mapcar (lambda (range)
                                         (gdocs-convert--list-prefix-length-for-element
                                          (plist-get range :element)))
                                       body-ranges))))
      (list :requests requests :index (- current removed-tabs)))))

;; ---------------------------------------------------------------------------
;;; Source block -> requests

(defun gdocs-convert--source-block-line-element (element line line-index line-count)
  "Return paragraph IR for source-block ELEMENT LINE at LINE-INDEX."
  (gdocs-convert--typographic-block-paragraph
   element
   (list (gdocs-convert--make-run line (gdocs-convert--code-props)))
   'gdocs-source-block
   (list :type 'src-block :data (or (plist-get element :language) ""))
   line-index
   line-count))

(defun gdocs-convert--example-block-line-element (element line line-index line-count)
  "Return paragraph IR for example-block ELEMENT LINE at LINE-INDEX."
  (gdocs-convert--typographic-block-paragraph
   element
   (list (gdocs-convert--make-run line (gdocs-convert--code-props)))
   'gdocs-source-block
   (list :type 'example-block :data (plist-get element :id))
   line-index
   line-count))

(defun gdocs-convert--example-block-to-requests (element index)
  "Create paragraph requests for example-block ELEMENT at INDEX."
  (let* ((lines (or (plist-get element :lines) '("")))
         (line-count (length lines))
         (line-index 0)
         (requests nil)
         (current index))
    (dolist (line lines)
      (let* ((paragraph (gdocs-convert--example-block-line-element
                         element line line-index line-count))
             (result (gdocs-convert--paragraph-to-requests paragraph current)))
        (setq requests (nconc requests (plist-get result :requests)))
        (setq current (plist-get result :index))
        (cl-incf line-index)))
    (list :requests requests :index current)))

(defun gdocs-convert--source-block-to-requests (element index)
  "Create paragraph requests for source-block ELEMENT at INDEX."
  (let* ((lines (or (plist-get element :lines) '("")))
         (line-count (length lines))
         (line-index 0)
         (requests nil)
         (current index))
    (dolist (line lines)
      (let* ((paragraph (gdocs-convert--source-block-line-element
                         element line line-index line-count))
             (result (gdocs-convert--paragraph-to-requests paragraph current)))
        (setq requests (nconc requests (plist-get result :requests)))
        (setq current (plist-get result :index))
        (cl-incf line-index)))
    (list :requests requests :index current)))

;; ---------------------------------------------------------------------------
;;; Image -> requests

(defun gdocs-convert--image-to-requests (element index)
  "Create insertInlineImage requests for image ELEMENT at INDEX.
ELEMENT must contain `:uri' or `:source-uri'.  Images without a fetchable
URI are left to adapter preflight code and produce no requests.
When ELEMENT has a `:caption', insert it as visible text in the following
paragraph for v1 semantic preservation."
  (if-let* ((uri (or (plist-get element :uri)
                    (plist-get element :source-uri))))
      (let* ((caption (plist-get element :caption))
             (caption-text (and caption (not (string-empty-p caption))
                                (concat "\n" caption "\n")))
             (caption-len (if caption-text
                              (gdocs-convert--string-to-utf16-length caption-text)
                            0))
             (caption-start (1+ index))
             (caption-end (+ caption-start caption-len))
             (caption-text-start (1+ caption-start))
             (caption-text-end (1- caption-end))
             (image-req `((insertInlineImage
                           . ((uri . ,uri)
                              (location . ((index . ,index)))))))
             (source-marker-req
              (gdocs-convert--make-image-source-marker-request element index))
             (caption-req (when caption-text
                            (gdocs-convert--make-insert-text-request
                             caption-text caption-start)))
             (caption-style-reqs
              (when caption-text
                (gdocs-convert--caption-style-requests
                 caption-start caption-end caption-text-start caption-text-end)))
             (caption-marker-reqs
              (when caption-text
                (gdocs-convert--make-image-caption-marker-requests
                 element caption-text-start caption-text-end))))
        (list :requests (append (list image-req)
                                (when source-marker-req (list source-marker-req))
                                (when caption-req (list caption-req))
                                caption-style-reqs
                                caption-marker-reqs)
              :index (+ index 1 caption-len)))
    (list :requests nil :index index)))

(defun gdocs-convert--make-image-source-marker-request (element index)
  "Create semantic marker request for image source ELEMENT at INDEX."
  (when-let* ((path (plist-get element :path)))
    (let ((id (or (plist-get element :id)
                  (plist-get element :object-id)
                  (format "image-%s" index))))
      `((createNamedRange
         . ((name . ,(format "gdocs-org-marker:image-source:%s:%s"
                             id (url-hexify-string path)))
            (range . ((startIndex . ,index)
                      (endIndex . ,(1+ index))))))))))

(defun gdocs-convert-make-logical-style-requests
    (style-name start-index end-index &optional text-start text-end segment-id)
  "Create paragraph/text requests for logical STYLE-NAME.
START-INDEX and END-INDEX target paragraph style in the body.  When
TEXT-START and TEXT-END are non-nil, also create text style requests.
Optional SEGMENT-ID targets the text style request at a non-body segment."
  (delq nil
        (list (gdocs-convert--logical-style-paragraph-request
               style-name start-index end-index)
              (when (and text-start text-end)
                (gdocs-convert--logical-style-text-request
                 style-name text-start text-end segment-id)))))

(defun gdocs-convert--caption-style-requests
    (start-index end-index text-start text-end)
  "Create logical image-caption style requests for caption ranges."
  (gdocs-convert-make-logical-style-requests
   'gdocs-image-caption start-index end-index text-start text-end))

(defun gdocs-convert--make-image-caption-marker-requests (element text-start text-end)
  "Create semantic marker requests for image caption ELEMENT.
TEXT-START and TEXT-END bound the visible caption text."
  (let ((id (or (plist-get element :id)
                (plist-get element :object-id)
                (format "image-%s" text-start))))
    (list `((createNamedRange
             . ((name . ,(format "gdocs-org-marker:image-caption:%s:%s"
                                  id gdocs-image-caption-style-name))
                (range . ((startIndex . ,text-start)
                          (endIndex . ,text-end)))))))))

;; ---------------------------------------------------------------------------
;;; Paragraph -> requests

(defun gdocs-convert--list-leading-prefix (element)
  "Return leading tab prefix for ELEMENT's list nesting level.
Google Docs `createParagraphBullets' determines native nesting by counting and
removing leading tabs, not by paragraph indentation alone."
  (let* ((list-info (plist-get element :list))
         (level (and list-info (or (plist-get list-info :level) 0))))
    (if (and level (> level 0))
        (make-string level ?\t)
      "")))

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
  (let* ((contents (plist-get element :contents))
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

(defun gdocs-convert--logical-paragraph-style-to-docs (style-plist)
  "Resolve logical paragraph STYLE-PLIST to Google Docs ParagraphStyle fields."
  (let ((style nil))
    (when-let ((alignment (plist-get style-plist :alignment)))
      (push `(alignment . ,(gdocs-convert--ir-alignment-to-docs alignment)) style))
    (when-let ((spacing-mode (plist-get style-plist :spacing-mode)))
      (push `(spacingMode . ,(pcase spacing-mode
                              ('never-collapse "NEVER_COLLAPSE")
                              ('collapse-lists "COLLAPSE_LISTS")
                              (_ nil)))
            style))
    (when-let ((space-above (plist-get style-plist :space-above)))
      (push `(spaceAbove . ((magnitude . ,space-above) (unit . "PT"))) style))
    (when-let ((space-below (plist-get style-plist :space-below)))
      (push `(spaceBelow . ((magnitude . ,space-below) (unit . "PT"))) style))
    (when-let ((indent-start (plist-get style-plist :indent-start)))
      (push `(indentStart . ((magnitude . ,indent-start) (unit . "PT"))) style))
    (when (plist-member style-plist :indent-first-line)
      (push `(indentFirstLine
              . ((magnitude . ,(plist-get style-plist :indent-first-line))
                 (unit . "PT")))
            style))
    (when-let ((indent-end (plist-get style-plist :indent-end)))
      (push `(indentEnd . ((magnitude . ,indent-end) (unit . "PT"))) style))
    (if-let ((borders (plist-get style-plist :borders)))
        (setq style (append (gdocs-convert--paragraph-border-styles
                             borders style-plist)
                            style))
      (when-let ((padding (plist-get style-plist :border-padding)))
        (setq style (append (gdocs-convert--paragraph-padding-border-style
                             padding
                             (or (plist-get style-plist :border-color)
                                 (plist-get style-plist :background-color)
                                 gdocs-style-default-border-color))
                            style))))
    (when-let ((background (plist-get style-plist :background-color)))
      (push `(shading . ((backgroundColor . ,(gdocs-convert--hex-to-color
                                              background))))
            style))
    (delq nil style)))

(defun gdocs-convert--paragraph-border-object (spec fallback-color fallback-padding)
  "Return a Google Docs ParagraphBorder from SPEC.
FALLBACK-COLOR and FALLBACK-PADDING are used when SPEC omits :color or
:padding.  SPEC may also provide :width in points."
  `((color . ,(gdocs-convert--hex-to-color
               (or (plist-get spec :color) fallback-color
                   gdocs-style-default-border-color)))
    (width . ((magnitude . ,(or (plist-get spec :width)
                               gdocs-style-default-border-width))
              (unit . "PT")))
    (padding . ((magnitude . ,(or (plist-get spec :padding) fallback-padding
                                gdocs-style-default-border-padding))
                (unit . "PT")))
    (dashStyle . "SOLID")))

(defun gdocs-convert--paragraph-border-field (side)
  "Return Google Docs paragraph border field for SIDE."
  (pcase side
    ('top 'borderTop)
    ('bottom 'borderBottom)
    ('left 'borderLeft)
    ('right 'borderRight)))

(defun gdocs-convert--paragraph-border-styles (borders style-plist)
  "Return paragraph border styles from BORDERS and STYLE-PLIST.
BORDERS is an alist of (SIDE . SPEC), where SIDE is one of `top', `bottom',
`left', or `right', and SPEC is a plist accepted by
`gdocs-convert--paragraph-border-object'."
  (let ((fallback-color (or (plist-get style-plist :border-color)
                            (plist-get style-plist :background-color)
                            gdocs-style-default-border-color))
        (fallback-padding (plist-get style-plist :border-padding)))
    (mapcar (lambda (entry)
              (cons (gdocs-convert--paragraph-border-field (car entry))
                    (gdocs-convert--paragraph-border-object
                     (cdr entry) fallback-color fallback-padding)))
            borders)))

(defun gdocs-convert--paragraph-padding-border-style (padding color)
  "Return paragraph borders with PADDING in points and COLOR."
  (let ((border (gdocs-convert--paragraph-border-object
                 (list :color color :width gdocs-style-default-border-width
                       :padding padding)
                 color padding)))
    `((borderTop . ,border)
      (borderBottom . ,border)
      (borderLeft . ,border)
      (borderRight . ,border))))

(defun gdocs-convert--style-field-mask-name (field)
  "Return Google Docs field mask name for STYLE FIELD."
  (pcase field
    ('shading "shading.backgroundColor")
    (_ (symbol-name field))))

(defun gdocs-convert--style-fields-mask-from-alist (style)
  "Return comma-separated fields mask for STYLE alist."
  (s-join "," (mapcar (lambda (pair)
                         (gdocs-convert--style-field-mask-name (car pair)))
                       style)))

(defun gdocs-convert--logical-style-paragraph-request (style-name start end)
  "Return logical STYLE-NAME paragraph request between START and END."
  (when-let* ((definition (gdocs-convert--style-definition style-name)))
    (let* ((parent (plist-get definition :parent))
           (paragraph-style (gdocs-convert--logical-paragraph-style-to-docs
                             (plist-get definition :paragraph)))
           (named-style (and parent (gdocs-convert--ir-style-to-docs parent)))
           (style (append (when named-style
                            `((namedStyleType . ,named-style)))
                          paragraph-style)))
      (when style
        `((updateParagraphStyle
           . ((paragraphStyle . ,style)
              (range . ((startIndex . ,start)
                        (endIndex . ,(1- end))))
              (fields . ,(gdocs-convert--style-fields-mask-from-alist
                          style)))))))))

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

(defun gdocs-convert--ir-alignment-to-docs (alignment)
  "Convert an IR ALIGNMENT symbol to a Google Docs alignment string."
  (pcase alignment
    ('center "CENTER")
    ('end "END")
    ('justified "JUSTIFIED")
    (_ nil)))

(defun gdocs-convert--ir-style-to-docs (style)
  "Convert an IR STYLE symbol to a Google Docs namedStyleType string."
  (or (gdocs-convert--heading-style-to-docs style)
      (pcase style
        ('title "TITLE")
        ('subtitle "SUBTITLE")
        (_ "NORMAL_TEXT"))))

(defun gdocs-convert--run-model-length (run)
  "Return Google Docs UTF-16 model length for RUN."
  (or (plist-get (gdocs-convert--inline-run-policy run) :model-length)
      (gdocs-convert--string-to-utf16-length (or (plist-get run :text) ""))))

(defun gdocs-convert--text-range (start end &optional segment-id)
  "Return a Google Docs text range from START to END.
When SEGMENT-ID is non-nil, target that segment instead of the body."
  (append (when segment-id (list (cons 'segmentId segment-id)))
          (list (cons 'startIndex start)
                (cons 'endIndex end))))

(defun gdocs-convert-make-text-style-requests (runs start-index &optional segment-id omit-status-markers)
  "Create updateTextStyle requests for formatted RUNS starting at START-INDEX.
Each run that has any formatting generates a request.  When SEGMENT-ID is
non-nil, requests target that document segment, such as a native footnote.
OMIT-STATUS-MARKERS suppresses duplicate markers for unchanged status tokens."
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

(defun gdocs-convert--style-definition (style-name)
  "Return logical style definition for STYLE-NAME."
  (cdr (assq style-name gdocs-style-definitions)))

(defun gdocs-convert--logical-text-style-to-docs (style-plist)
  "Resolve logical text STYLE-PLIST to Google Docs TextStyle fields."
  (let ((style nil))
    (let ((font-family (or (plist-get style-plist :font-family)
                           (when (plist-get style-plist :monospace)
                             (car gdocs-convert-monospace-font-families))))
          (font-weight (plist-get style-plist :font-weight)))
      (when font-family
        (push `(weightedFontFamily
                . ,(append `((fontFamily . ,font-family))
                            (when font-weight
                              `((weight . ,font-weight)))))
              style)))
    (when (plist-member style-plist :bold)
      (push `(bold . ,(plist-get style-plist :bold)) style))
    (when (plist-member style-plist :italic)
      (push `(italic . ,(plist-get style-plist :italic)) style))
    (when (plist-member style-plist :underline)
      (push `(underline . ,(plist-get style-plist :underline)) style))
    (when-let ((fg (plist-get style-plist :foreground-color)))
      (push `(foregroundColor . ,(gdocs-convert--hex-to-color fg)) style))
    (when-let ((bg (plist-get style-plist :background-color)))
      (push `(backgroundColor . ,(gdocs-convert--hex-to-color bg)) style))
    (when-let ((scale (plist-get style-plist :font-scale)))
      (push `(fontSize . ((magnitude . ,(* gdocs-style-base-font-size scale))
                          (unit . "PT")))
            style))
    (when-let ((size (plist-get style-plist :font-size)))
      (push `(fontSize . ((magnitude . ,size) (unit . "PT"))) style))
    style))

(defun gdocs-convert--logical-style-text-fields (style-name)
  "Return Google Docs TextStyle fields for logical STYLE-NAME."
  (gdocs-convert--logical-text-style-to-docs
   (plist-get (gdocs-convert--style-definition style-name) :text)))

(defun gdocs-convert--logical-style-text-request (style-name start end
                                                             &optional segment-id)
  "Return logical STYLE-NAME text style request from START to END.
When SEGMENT-ID is non-nil, target that non-body segment."
  (let ((style (gdocs-convert--logical-style-text-fields style-name)))
    (when style
      `((updateTextStyle
         . ((textStyle . ,style)
            (range . ,(gdocs-convert--text-range start end segment-id))
            (fields . ,(gdocs-convert--style-fields-mask-from-alist
                        style))))))))

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

(defun gdocs-convert--hex-to-color (hex)
  "Convert HEX color string (#RRGGBB) to a Google Docs color alist."
  (let ((r (/ (string-to-number (substring hex 1 3) 16) 255.0))
        (g (/ (string-to-number (substring hex 3 5) 16) 255.0))
        (b (/ (string-to-number (substring hex 5 7) 16) 255.0)))
    `((color . ((rgbColor . ((red . ,r) (green . ,g) (blue . ,b))))))))

(defun gdocs-convert--style-fields-mask (style-fields)
  "Compute the fields mask string for STYLE-FIELDS alist."
  (let ((fields nil))
    (dolist (pair style-fields)
      (push (symbol-name (car pair)) fields))
    (s-join "," (nreverse fields))))

(defun gdocs-convert--make-list-requests (element start end)
  "Create bullet/list requests for ELEMENT from START to END."
  (let ((list-info (plist-get element :list)))
    (when list-info
      (let* ((type (plist-get list-info :type))
             (level (plist-get list-info :level))
             (preset (gdocs-convert--list-type-to-preset type))
             (bullet-req `((createParagraphBullets
                            . ((range . ((startIndex . ,start)
                                         (endIndex . ,(1- end))))
                               (bulletPreset . ,preset)))))
             (indent-req (gdocs-convert--make-list-indent-request
                          level start end)))
        (if indent-req
            (list indent-req bullet-req)
          (list bullet-req))))))

(defun gdocs-convert--make-list-indent-request (level start end)
  "Create an updateParagraphStyle request for list nesting LEVEL.
START and END define the paragraph range.  Returns nil for level 0,
since `createParagraphBullets' already sets the correct indentation.

IMPORTANT: This request must appear BEFORE the corresponding
`createParagraphBullets' request in the batchUpdate sequence.
Google Docs reads pre-existing paragraph indentation when creating
bullets and uses it to determine the nesting level."
  (when (> level 0)
    ;; indentStart = left margin for continuation (wrapped) lines.
    ;; indentFirstLine = bullet glyph position, one half-indent less.
    (let ((indent-start (* gdocs-convert--list-indent-pt (1+ level)))
          (indent-first (+ (/ gdocs-convert--list-indent-pt 2)
                           (* gdocs-convert--list-indent-pt level))))
      `((updateParagraphStyle
         . ((paragraphStyle
             . ((indentStart . ((magnitude . ,indent-start)
                                (unit . "PT")))
                (indentFirstLine . ((magnitude . ,indent-first)
                                    (unit . "PT")))))
            (range . ((startIndex . ,start)
                      (endIndex . ,(1- end))))
            (fields . "indentStart,indentFirstLine")))))))

(defun gdocs-convert--list-type-to-preset (type)
  "Convert an IR list TYPE to a Google Docs bullet preset string."
  (pcase type
    ('bullet "BULLET_DISC_CIRCLE_SQUARE")
    ('number "NUMBERED_DECIMAL_ALPHA_ROMAN")
    ('check "BULLET_CHECKBOX")))

(defconst gdocs-convert--numbered-presets
  '("NUMBERED_DECIMAL_ALPHA_ROMAN" "NUMBERED_DECIMAL_NESTED")
  "Numbered bullet presets that look identical at nesting level 0.
Used to alternate presets between consecutive numbered list groups,
preventing Google Docs from merging them in a single batchUpdate.")

(defun gdocs-convert--alternate-numbered-presets (groups)
  "Alternate presets for consecutive numbered list GROUPS.
Mutates the :preset of each group in place.  Consecutive groups
whose preset starts with \"NUMBERED_\" receive alternating
presets so that Google Docs creates separate list objects."
  (let ((prev-preset nil))
    (dolist (group groups)
      (let ((preset (plist-get group :preset)))
        (when (and (string= preset prev-preset)
                   (string-prefix-p "NUMBERED_" preset))
          (plist-put group :preset
                     (gdocs-convert--next-numbered-preset preset)))
        (setq prev-preset (plist-get group :preset))))))

(defun gdocs-convert--next-numbered-preset (preset)
  "Return the next numbered preset after PRESET, cycling through options."
  (let* ((idx (seq-position gdocs-convert--numbered-presets preset))
         (next (% (1+ (or idx 0)) (length gdocs-convert--numbered-presets))))
    (nth next gdocs-convert--numbered-presets)))

(defun gdocs-convert--fixup-list-nesting (requests element-ranges)
  "Fix list nesting in REQUESTS by merging per-paragraph bullet requests.
ELEMENT-RANGES is a list of plists (:element EL :start S :end E) tracking
where each IR element was placed.

The Google Docs API only assigns correct nesting levels when
`createParagraphBullets' covers an entire contiguous list group in
a single request, with indentation pre-set on nested items.
Per-paragraph bullet requests produce flat lists.

This function removes per-paragraph `createParagraphBullets' and
appends a single merged request per contiguous list group.
Consecutive numbered groups receive alternating presets to prevent
Google Docs from merging them into one continuous list."
  (let ((groups (gdocs-convert--find-list-groups element-ranges)))
    (if (null groups)
        requests
      (let ((filtered (cl-remove-if
                       (lambda (req)
                         (and (alist-get 'createParagraphBullets req)
                              (gdocs-convert--bullet-request-in-list-groups-p
                               req groups)))
                       requests)))
        (gdocs-convert--alternate-numbered-presets groups)
        (append filtered
                ;; Leading tabs are removed by createParagraphBullets, so run
                ;; later list groups first to keep their pre-removal indices
                ;; valid.
                (mapcar #'gdocs-convert--list-group-bullet-request
                        (reverse groups))
                (gdocs-convert--callout-list-post-bullet-indent-resets
                 element-ranges)
                (gdocs-convert--guard-non-list-paragraphs
                 element-ranges groups))))))

(defun gdocs-convert--bullet-request-in-list-groups-p (request groups)
  "Return non-nil if bullet REQUEST belongs to one of GROUPS."
  (when-let* ((bullet (alist-get 'createParagraphBullets request))
              (range (alist-get 'range bullet))
              (start (alist-get 'startIndex range))
              (end (alist-get 'endIndex range)))
    (seq-some (lambda (group)
                (and (>= start (plist-get group :start))
                     (<= end (1- (plist-get group :end)))))
              groups)))

(defun gdocs-convert--find-list-groups (element-ranges)
  "Find contiguous groups of list elements in ELEMENT-RANGES.
Returns a list of groups, where each group is a plist with
:start (index of first element), :end (index after last element),
and :preset (bullet preset string)."
  (let ((groups nil)
        (current-start nil)
        (current-end nil)
        (current-preset nil))
    (dolist (range element-ranges)
      (let* ((elem (plist-get range :element))
             (list-info (plist-get elem :list)))
        (if list-info
            (let ((preset (gdocs-convert--list-type-to-preset
                           (plist-get list-info :type))))
              (if (and current-start
                       (string= preset current-preset))
                  ;; Extend current group
                  (setq current-end (plist-get range :end))
                ;; Flush previous group and start new one
                (when current-start
                  (push (list :start current-start
                              :end current-end
                              :preset current-preset)
                        groups))
                (setq current-start (plist-get range :start)
                      current-end (plist-get range :end)
                      current-preset preset)))
          ;; Non-list element: flush current group
          (when current-start
            (push (list :start current-start
                        :end current-end
                        :preset current-preset)
                  groups)
            (setq current-start nil
                  current-end nil
                  current-preset nil)))))
    ;; Flush final group
    (when current-start
      (push (list :start current-start
                  :end current-end
                  :preset current-preset)
            groups))
    (nreverse groups)))

(defun gdocs-convert--callout-list-marker-p (element)
  "Return non-nil when ELEMENT is a callout list paragraph."
  (seq-some (lambda (marker)
              (eq (plist-get marker :type) 'callout-list))
            (plist-get element :gdocs-marker)))

(defun gdocs-convert--list-prefix-length-for-element (element)
  "Return leading tab count inserted for ELEMENT."
  (if (plist-get element :list)
      (length (gdocs-convert--list-leading-prefix element))
    0))

(defun gdocs-convert--list-prefixes-before (range element-ranges)
  "Return number of list prefix tabs before RANGE in ELEMENT-RANGES."
  (let ((start (plist-get range :start))
        (total 0))
    (dolist (candidate element-ranges total)
      (when (< (plist-get candidate :start) start)
        (setq total (+ total
                       (gdocs-convert--list-prefix-length-for-element
                        (plist-get candidate :element))))))))

(defun gdocs-convert--callout-list-post-bullet-indent-resets (element-ranges)
  "Return final indent resets for callout list paragraphs.
These run after `createParagraphBullets'.  They keep paragraph shading
full-width inside callout panels while native nesting level has already
been assigned from leading tabs."
  (let (requests)
    (dolist (range element-ranges (nreverse requests))
      (let* ((element (plist-get range :element))
             (level-tabs (gdocs-convert--list-prefix-length-for-element element)))
        (when (and (plist-get element :list)
                   (zerop level-tabs)
                   (gdocs-convert--callout-list-marker-p element))
          (let* ((removed-before (gdocs-convert--list-prefixes-before
                                  range element-ranges))
                 (start (- (plist-get range :start) removed-before))
                 (end (- (plist-get range :end) removed-before level-tabs)))
            (push (gdocs-convert--callout-list-indent-reset-request start end)
                  requests)))))))

(defun gdocs-convert--list-group-bullet-request (group)
  "Create a single createParagraphBullets request for GROUP.
GROUP is a plist with :start, :end, and :preset."
  (let ((start (plist-get group :start))
        (end (plist-get group :end))
        (preset (plist-get group :preset)))
    `((createParagraphBullets
       . ((range . ((startIndex . ,start)
                    (endIndex . ,(1- end))))
          (bulletPreset . ,preset))))))

(defun gdocs-convert--guard-non-list-paragraphs (element-ranges groups)
  "Generate `deleteParagraphBullets' for non-list paragraphs near GROUPS.
ELEMENT-RANGES is the full list of element ranges.  GROUPS is the
list of list groups from `gdocs-convert--find-list-groups'.

The Google Docs API can extend bullet formatting to heading
paragraphs adjacent to `createParagraphBullets' ranges.  This
function emits defensive `deleteParagraphBullets' requests for
every non-list paragraph that sits between consecutive list
groups, immediately before the first group, or immediately after
the last group."
  (when groups
    (let ((requests nil)
          (gaps (gdocs-convert--list-group-gaps element-ranges groups)))
      (dolist (range element-ranges)
        (let* ((elem (plist-get range :element))
               (start (plist-get range :start))
               (end (plist-get range :end)))
          (when (and (eq (plist-get elem :type) 'paragraph)
                     (not (plist-get elem :list))
                     (gdocs-convert--in-any-gap-p start end gaps))
            ;; These guards run after createParagraphBullets.  Google Docs
            ;; removes leading tabs used to create native nested levels, so
            ;; guard ranges after list items must be shifted left by the count
            ;; of removed list-prefix tabs before the guarded paragraph.
            (let* ((removed-before (gdocs-convert--list-prefixes-before
                                    range element-ranges))
                   (adjusted-start (- start removed-before))
                   (adjusted-end (- end removed-before)))
              (when (< adjusted-start adjusted-end)
                (push `((deleteParagraphBullets
                         . ((range
                             . ((startIndex . ,adjusted-start)
                                (endIndex . ,(1- adjusted-end)))))))
                      requests))))))
      (nreverse requests))))

(defun gdocs-convert--list-group-gaps (element-ranges groups)
  "Compute gap ranges around and between GROUPS.
ELEMENT-RANGES provides the document extent.  Returns a list of
\(GAP-START . GAP-END) cons cells covering the region immediately
before the first group, between consecutive groups, and
immediately after the last group."
  (let ((gaps nil)
        (first-group (car groups))
        (last-group (car (last groups))))
    (cl-loop for (g1 g2) on groups
             while g2
             do (push (cons (plist-get g1 :end)
                            (plist-get g2 :start))
                      gaps))
    (gdocs-convert--add-edge-gaps
     gaps first-group last-group element-ranges)))

(defun gdocs-convert--add-edge-gaps (gaps first-group last-group ranges)
  "Add edge gaps before FIRST-GROUP and after LAST-GROUP to GAPS.
RANGES provides the document extent.  Returns the augmented gap list."
  (let ((doc-start (plist-get (car ranges) :start))
        (doc-end (plist-get (car (last ranges)) :end)))
    (when (> (plist-get first-group :start) doc-start)
      (push (cons doc-start (plist-get first-group :start)) gaps))
    (when (< (plist-get last-group :end) doc-end)
      (push (cons (plist-get last-group :end) doc-end) gaps))
    gaps))

(defun gdocs-convert--in-any-gap-p (start end gaps)
  "Return non-nil if the range START..END falls within any gap in GAPS."
  (cl-some (lambda (gap)
             (and (>= start (car gap))
                  (<= end (cdr gap))))
           gaps))

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
;;; Table -> requests

(defconst gdocs-convert--table-page-width-pt 468
  "Usable page width in points for table column width calculation.
Based on standard Google Docs Letter page (612pt) with 1-inch
margins (72pt each side): 612 - 72 - 72 = 468.")

(defconst gdocs-convert--table-min-column-width-pt 36
  "Minimum column width in points.
Prevents columns from becoming unreadably narrow.  The Google
Docs API requires at least 5pt, but 36pt (0.5 inch) is a
practical minimum for readability.")

(defconst gdocs-convert--table-insert-overhead 3
  "Structural overhead of `insertTable' beyond the table itself.
Accounts for: preserved paragraph (+1, insertTable keeps the
paragraph at the insertion point), table-start (+1), table-end (+1).
See `gdocs-diff--table-utf16-length' for the read-side formula,
which uses 2 instead of 3 because the preserved paragraph is not
part of the table itself.")

(defun gdocs-convert--table-to-requests (element index)
  "Convert an IR table ELEMENT to requests starting at INDEX.
Returns a plist (:requests LIST :index NEW-INDEX).
After the insertTable request, generates updateTableColumnProperties
requests to set proportional column widths based on content, then
insertText requests for each non-empty cell (processed last-to-first
so indices stay stable)."
  (let* ((rows (plist-get element :rows))
         (nrows (length rows))
         (ncols (length (car rows)))
         (insert-req `((insertTable
                        . ((rows . ,nrows)
                           (columns . ,ncols)
                           (location . ((index . ,index)))))))
         (cell-text-len (gdocs-convert--table-total-cell-text-length rows))
         ;; Per-row: 1 row-start marker + ncols * 2 (cell-start + cell-newline).
         (table-overhead (+ gdocs-convert--table-insert-overhead
                            (* nrows (+ 1 (* ncols 2)))
                            cell-text-len))
         (new-index (+ index table-overhead))
         ;; Column widths: compute proportional widths from content.
         ;; Table start is at index + 1 (insertTable preserves the
         ;; paragraph at the insertion point).
         (col-widths (gdocs-convert--table-column-widths
                      (gdocs-convert--table-column-max-lengths rows)))
         (col-width-reqs (gdocs-convert--table-column-width-requests
                          (1+ index) ncols col-widths))
         (cell-reqs (gdocs-convert--table-cell-requests rows index ncols))
         (requests (append (list insert-req) col-width-reqs cell-reqs)))
    (list :requests requests
          :index new-index)))

(defconst gdocs-convert--table-base-cell-offset 4
  "Offset from `insertTable' index to the first cell's content.
Accounts for: preserved paragraph (+1), table-start structural
element (+1), first row-start marker (+1), first cell-start
marker (+1).")

(defun gdocs-convert--table-cell-requests (rows table-index ncols)
  "Generate insertText and text-style-reset requests for cells in ROWS.
TABLE-INDEX is the insertTable location index.  NCOLS is the
column count.  The table body starts at TABLE-INDEX + 1 because
insertTable preserves the existing paragraph at the location.

For each non-empty cell, generates an insertText followed by a
text-style reset to clear inherited formatting (font size, font
family, colors).  Without the reset, cells inherit the style at
the insertion point, which may be a heading or other non-body
style.

Iterates forward and pushes, so the result is in reverse order
\(last cell first) for stable index processing."
  (let ((reqs nil)
        (r 0))
    (dolist (row rows)
      (let ((c 0))
        (dolist (cell row)
          (let* ((text (gdocs-convert--runs-to-plain-text cell))
                 (text-len (gdocs-convert--string-to-utf16-length text)))
            (when (> text-len 0)
              (let ((cell-start (+ table-index
                                   gdocs-convert--table-base-cell-offset
                                   (* r (+ 1 (* ncols 2)))
                                   (* c 2))))
                ;; Reset formatting so cells don't inherit heading styles or
                ;; other non-body formatting, then reapply semantic cell-run
                ;; formatting.  Push style requests first, then reset, then
                ;; insertText, so the returned last-to-first batch order is:
                ;; insertText -> reset -> semantic styles.
                (dolist (style-req (reverse
                                     (gdocs-convert--make-text-style-requests
                                      cell cell-start)))
                  (push style-req reqs))
                (push (gdocs-convert--make-text-style-reset-request
                       cell-start (+ cell-start text-len))
                      reqs)
                (push (gdocs-convert--make-insert-text-request
                       text cell-start)
                      reqs))))
          (setq c (1+ c))))
      (setq r (1+ r)))
    reqs))

(defun gdocs-convert--table-total-cell-text-length (rows)
  "Return total UTF-16 length of all cell text in ROWS."
  (let ((total 0))
    (dolist (row rows)
      (dolist (cell row)
        (setq total (+ total (gdocs-convert--string-to-utf16-length
                              (gdocs-convert--runs-to-plain-text cell))))))
    total))

(defun gdocs-convert--table-column-max-lengths (rows)
  "Return a list of maximum text lengths per column in ROWS.
Each entry is the character count of the longest cell text in
that column, used as a proxy for visual width."
  (let* ((ncols (length (car rows)))
         (maxes (make-list ncols 0)))
    (dolist (row rows)
      (let ((c 0))
        (dolist (cell row)
          (when (< c ncols)
            (let ((len (length (gdocs-convert--runs-to-plain-text cell))))
              (when (> len (nth c maxes))
                (setf (nth c maxes) len))))
          (setq c (1+ c)))))
    maxes))

(defun gdocs-convert--table-column-widths (max-lengths)
  "Convert MAX-LENGTHS to proportional column widths in points.
Distributes `gdocs-convert--table-page-width-pt' proportionally
based on max content lengths.  Columns whose proportional share
falls below `gdocs-convert--table-min-column-width-pt' receive
the minimum; the remaining page width is distributed among the
larger columns so that the total sums exactly to page width."
  (let* ((ncols (length max-lengths))
         (page-width (float gdocs-convert--table-page-width-pt))
         (min-width (min (float gdocs-convert--table-min-column-width-pt)
                         (/ page-width ncols)))
         (total-len (apply #'+ max-lengths)))
    (if (zerop total-len)
        (make-list ncols (/ page-width ncols))
      (let* ((proportional (mapcar (lambda (len)
                                     (* (/ (float len) total-len) page-width))
                                   max-lengths))
             (min-count 0)
             (remaining-prop-total 0.0))
        (dolist (w proportional)
          (if (< w min-width)
              (setq min-count (1+ min-count))
            (setq remaining-prop-total (+ remaining-prop-total w))))
        (let ((remaining-width (- page-width (* min-count min-width))))
          (if (<= remaining-prop-total 0)
              (make-list ncols (/ page-width ncols))
            (mapcar (lambda (w)
                      (if (< w min-width)
                          min-width
                        (* (/ w remaining-prop-total) remaining-width)))
                    proportional)))))))

(defun gdocs-convert--table-column-width-requests (table-start-index ncols widths)
  "Generate updateTableColumnProperties requests.
TABLE-START-INDEX is the document index of the table start element.
NCOLS is the number of columns.  WIDTHS is a list of widths in points."
  (let ((reqs nil))
    (dotimes (c ncols)
      (push `((updateTableColumnProperties
               . ((tableStartLocation
                   . ((index . ,table-start-index)))
                  (columnIndices . ,(vector c))
                  (tableColumnProperties
                   . ((widthType . "FIXED_WIDTH")
                      (width . ((magnitude . ,(nth c widths))
                                (unit . "PT")))))
                  (fields . "*"))))
            reqs))
    (nreverse reqs)))

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
