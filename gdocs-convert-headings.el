;;; gdocs-convert-headings.el --- Heading semantics and canonicalization -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pablo Stafforini

;;; Commentary:
;; Org and Docs heading conversion.  Presentation comes from gdocs-styles;
;; heading levels, metadata, and visible inline semantics are conversion rules.
;; Shared run and marker helpers are supplied by the gdocs-convert dispatcher.

;;; Code:

(require 'org)
(require 'org-element)
(require 'seq)
(require 'subr-x)

(declare-function gdocs-convert--objects-to-runs "gdocs-convert" (objects &optional inherited-props))
(declare-function gdocs-convert--next-id "gdocs-convert" ())
(declare-function gdocs-convert--extract-marker-field "gdocs-convert" (marker type-sym))
(declare-function gdocs-convert--merge-adjacent-runs "gdocs-convert" (runs))

(defun gdocs-convert--headline-to-ir (headline)
  "Convert an org HEADLINE element to an IR element."
  (let* ((level (org-element-property :level headline))
         (style (gdocs-convert--level-to-style level))
         (title-objects (org-element-property :title headline))
         (contents (gdocs-convert--objects-to-runs title-objects))
         (marker (gdocs-convert--headline-marker headline)))
    (append (list :type 'paragraph
                  :style style
                  :style-name (gdocs-convert--heading-logical-style-name style)
                  :contents contents
                  :id (gdocs-convert--next-id))
            (when marker (list :gdocs-marker marker)))))

(defun gdocs-convert--heading-logical-style-name (style)
  "Return logical Google Docs style name for heading STYLE."
  (intern (format "gdocs-%s" style)))

(defun gdocs-convert--level-to-style (level)
  "Convert an org heading LEVEL to an IR style, clamped to heading-6."
  (intern (format "heading-%d" (min level 6))))

(defun gdocs-convert--headline-marker (headline)
  "Return preservable Org-only metadata from HEADLINE, or nil."
  (let* ((todo (org-element-property :todo-keyword headline))
         (tags (org-element-property :tags headline))
         (priority (org-element-property :priority headline))
         (scheduled (org-element-property :scheduled headline))
         (deadline (org-element-property :deadline headline))
         (parts nil))
    (when deadline
      (push (list :type 'deadline :data (org-element-interpret-data deadline)) parts))
    (when scheduled
      (push (list :type 'scheduled :data (org-element-interpret-data scheduled)) parts))
    (when priority
      (push (list :type 'priority :data (char-to-string priority)) parts))
    (when tags
      (push (list :type 'tags :data (string-join tags ":")) parts))
    (when todo
      (push (list :type 'todo :data todo) parts))
    parts))

(defun gdocs-convert--heading-style-p (style)
  "Return non-nil if STYLE is a heading style."
  (and style (string-prefix-p "heading-" (symbol-name style))))

(defun gdocs-convert--heading-docs-to-style (named-style)
  "Return a heading IR style for Docs NAMED-STYLE, or nil."
  (let ((case-fold-search nil))
    (when (and named-style (string-match "\\`HEADING_\\([1-6]\\)\\'" named-style))
      (intern (concat "heading-" (match-string 1 named-style))))))

(defun gdocs-convert--heading-style-to-docs (style)
  "Return the Docs named heading style for IR STYLE, or nil."
  (let ((case-fold-search nil))
    (when (and style (string-match "\\`heading-\\([1-6]\\)\\'" (symbol-name style)))
      (concat "HEADING_" (match-string 1 (symbol-name style))))))

(defun gdocs-convert--format-heading (style text marker)
  "Format a heading with STYLE, TEXT, and optional Org-only MARKER data."
  (let* ((level (string-to-number (substring (symbol-name style) (length "heading-"))))
         (stars (make-string level ?*))
         (todo (gdocs-convert--extract-marker-field marker 'todo))
         (tags (gdocs-convert--extract-marker-field marker 'tags))
         (priority (gdocs-convert--extract-marker-field marker 'priority))
         (head (concat stars " " (when todo (concat todo " "))
                       (when priority (format "[#%s] " priority)) text))
         (scheduled (gdocs-convert--extract-marker-field marker 'scheduled))
         (deadline (gdocs-convert--extract-marker-field marker 'deadline))
         (planning-parts nil))
    (when deadline (push (format "DEADLINE: %s" deadline) planning-parts))
    (when scheduled (push (format "SCHEDULED: %s" scheduled) planning-parts))
    (let ((heading (if tags (gdocs-convert--heading-with-tags head tags) head)))
      (if planning-parts
          (concat heading "\n" (string-join planning-parts " "))
        heading))))

(defun gdocs-convert--heading-with-tags (head tags-string)
  "Format HEAD with TAGS-STRING in Org syntax."
  (format "%s :%s:" head tags-string))

(defun gdocs-convert--normalize-heading-bold (runs)
  "Treat uniform heading bold in RUNS as presentation, not inline emphasis.
Keep contrasting partial bold and all other inline formatting.  Ignore plain
whitespace runs introduced by text-run normalization when checking uniformity."
  (let ((visible (seq-filter
                  (lambda (run)
                    (and (not (plist-get run :status))
                         (not (string-blank-p (or (plist-get run :text) "")))))
                  runs)))
    (if (and visible (seq-every-p (lambda (run) (plist-get run :bold)) visible))
        (gdocs-convert--merge-adjacent-runs
         (mapcar (lambda (run) (plist-put (copy-sequence run) :bold nil)) runs))
      runs)))

(provide 'gdocs-convert-headings)
;;; gdocs-convert-headings.el ends here
