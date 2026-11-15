;;; gdocs-convert-org-blocks.el --- Org paragraph block adapters -*- lexical-binding: t; -*-

;;; Commentary:
;; Paragraph-level Org block recognition/rendering belongs here, not in the
;; plain paragraph codec.  Document block assembly and styling are separate.

;;; Code:

(require 'cl-lib)
(require 'org-element)
(require 's)
(require 'subr-x)
(require 'gdocs-convert-paragraph-codec)

(declare-function gdocs-convert--extract-marker-field "gdocs-convert" (marker type-sym))
(declare-function gdocs-convert--plain-text "gdocs-convert" (element))

(defun gdocs-convert-code-paragraph-to-ir (runs)
  "Return code paragraph elements for RUNS, or nil when unhandled."
  (when (gdocs-convert--multiline-code-paragraph-p runs)
    (gdocs-convert--split-code-paragraph-runs runs)))

(defun gdocs-convert-block-paragraph-to-ir (paragraph runs)
  "Return contextual block IR for PARAGRAPH and RUNS, or nil when unhandled."
  (when (and runs (eq (gdocs-convert--paragraph-style paragraph) 'quote))
    (list (gdocs-convert-paragraph-from-runs runs 'quote))))

(defun gdocs-convert--paragraph-style (paragraph)
  "Determine the IR style for PARAGRAPH based on its context."
  (let ((parent (org-element-parent paragraph)))
    (if (eq (org-element-type parent) 'quote-block) 'quote 'normal)))

(defun gdocs-convert--multiline-code-paragraph-p (runs)
  "Return non-nil if RUNS represent consecutive inline code lines."
  (and runs
       (cl-some (lambda (run)
                  (and (not (plist-get run :code))
                       (string-match-p "\n" (plist-get run :text)))) runs)
       (cl-every (lambda (run)
                   (or (plist-get run :code) (string-blank-p (plist-get run :text)))) runs)))

(defun gdocs-convert--split-code-paragraph-runs (runs)
  "Split multi-line inline code RUNS into individual code paragraphs."
  (let* ((full-text (mapconcat (lambda (run) (plist-get run :text)) runs ""))
         (lines (split-string full-text "\n"))
         (lines (if (and lines (string-empty-p (car (last lines)))) (butlast lines) lines)))
    (mapcar (lambda (line)
              (gdocs-convert-paragraph-from-runs
               (list (gdocs-convert--make-run line (gdocs-convert--code-props))))) lines)))

(defun gdocs-convert--code-props (&optional inherited-props)
  "Return code formatting props derived from INHERITED-PROPS."
  (let ((props (copy-sequence inherited-props)))
    (setq props (plist-put props :code t))
    (plist-put props :style-names (cons 'gdocs-code (plist-get props :style-names)))))

(defun gdocs-convert-block-paragraph-to-org (element text)
  "Render block paragraph ELEMENT and TEXT, or nil when unhandled."
  (let ((style (plist-get element :style)) (marker (plist-get element :gdocs-marker)))
    (cond
     ((eq style 'quote) (format "#+BEGIN_QUOTE\n%s\n#+END_QUOTE" text))
     ((when-let* ((data (gdocs-convert--extract-marker-field marker 'src-block)))
        (gdocs-convert--format-src-block data element)))
     ((when-let* ((data (gdocs-convert--extract-marker-field marker 'keyword)))
        (format "#+%s: %s" (plist-get data :key) (plist-get data :value))))
     ((gdocs-convert--extract-marker-field marker 'verse-block)
      (format "#+BEGIN_VERSE\n%s\n#+END_VERSE" text))
     ((when-let* ((block-type (gdocs-convert--extract-marker-field marker 'special-block)))
        (format "#+BEGIN_%s\n%s\n#+END_%s" block-type text block-type)))
     ((when-let* ((data (gdocs-convert--extract-marker-field marker 'latex-environment))) data))
     ((when-let* ((data (gdocs-convert--extract-marker-field marker 'drawer)))
        (s-trim (plist-get data :raw))))
     ((eq (plist-get element :alignment) 'center)
      (format "#+BEGIN_CENTER\n%s\n#+END_CENTER" text)))))

(defun gdocs-convert--format-src-block-from-lines (language lines)
  "Format a source block with LANGUAGE from code LINES."
  (concat "#+BEGIN_SRC" (if (s-present? language) (concat " " language) "")
          "\n" (s-join "\n" lines) "\n#+END_SRC"))

(defun gdocs-convert--format-src-block (data element)
  "Format a src block from marker DATA and paragraph ELEMENT."
  (let* ((legacy-data-p (listp data))
         (language (if legacy-data-p (plist-get data :language) data))
         (value (if legacy-data-p (plist-get data :value)
                  (concat (gdocs-convert--plain-text element) "\n"))))
    (gdocs-convert--format-src-block-from-lines
     language (split-string (s-trim-right value) "\n"))))

(defvar gdocs-convert-monospace-font-families)

(defun gdocs-convert-code-source-object (object props)
  "Decode code-like OBJECT with inherited PROPS, or nil when unhandled."
  (pcase (org-element-type object)
    ((or 'code 'verbatim) (gdocs-convert--code-object-to-run object props))
    ('latex-fragment (gdocs-convert--latex-fragment-to-runs object props))
    ('inline-src-block (gdocs-convert--inline-src-block-to-runs object props))))

(defun gdocs-convert--code-object-to-run (object inherited-props)
  "Convert code OBJECT to runs with INHERITED-PROPS."
  (list (gdocs-convert--make-run (org-element-property :value object)
                                 (gdocs-convert--code-props inherited-props))))

(defun gdocs-convert--latex-fragment-to-runs (object inherited-props)
  "Convert formula OBJECT to literal code runs with INHERITED-PROPS."
  (list (gdocs-convert--make-run (org-element-property :value object)
                                 (gdocs-convert--code-props inherited-props))))

(defun gdocs-convert--inline-src-block-to-runs (object inherited-props)
  "Convert inline source OBJECT to code runs with INHERITED-PROPS."
  (list (gdocs-convert--make-run (org-element-property :value object)
                                 (gdocs-convert--code-props inherited-props))))

(defun gdocs-convert--docs-is-monospace-p (style)
  "Return non-nil if native STYLE uses a configured code font."
  (let ((family (alist-get 'fontFamily (alist-get 'weightedFontFamily style))))
    (and family (member family gdocs-convert-monospace-font-families))))

(provide 'gdocs-convert-org-blocks)
;;; gdocs-convert-org-blocks.el ends here
