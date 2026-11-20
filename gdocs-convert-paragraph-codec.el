;;; gdocs-convert-paragraph-codec.el --- Bidirectional plain paragraphs -*- lexical-binding: t; -*-

;;; Commentary:
;; Plain paragraph conversion composes generic inline services.  Feature owners
;; supply paragraph roles, extra attributes and optional Org renderers.  This
;; module does not recognize headings, lists, native objects or Org block kinds.
;; Literal construction remains in gdocs-convert-paragraphs.

;;; Code:

(require 'org-element)
(require 'seq)
(require 'gdocs-convert-paragraphs)
(require 'gdocs-convert-inline-codec)

(declare-function gdocs-convert--trim-runs "gdocs-convert" (runs))
(declare-function gdocs-convert--next-id "gdocs-convert" ())
(declare-function gdocs-convert--paragraph-markers-for-range "gdocs-convert" (markers start end))

(defun gdocs-convert-paragraph-org-runs (paragraph)
  "Return trimmed inline runs from Org PARAGRAPH, or nil if empty."
  (gdocs-convert--trim-runs
   (gdocs-convert--objects-to-runs (org-element-contents paragraph))))

(defun gdocs-convert-paragraph-from-runs (runs &optional style extra)
  "Return paragraph IR from RUNS, STYLE and ordered EXTRA attributes.
Return nil for empty RUNS.  Callers own role recognition and ID allocation is
shared with the document walk.  Attribute order preserves the existing IR."
  (when runs
    (append (list :type 'paragraph :style (or style 'normal)
                  :contents runs :id (gdocs-convert--next-id)) extra)))

(defun gdocs-convert--paragraph-child-to-ir (paragraph)
  "Convert Org PARAGRAPH child to paragraph IR, or nil if empty."
  (gdocs-convert-paragraph-from-runs (gdocs-convert-paragraph-org-runs paragraph)))

(defun gdocs-convert-paragraph-docs-runs (paragraph markers &optional literal-whitespace)
  "Decode Docs PARAGRAPH using MARKERS and optional LITERAL-WHITESPACE.
Strip the paragraph terminator, not authored internal line breaks."
  (gdocs-convert--trim-trailing-newline-runs
   (gdocs-convert--docs-elements-to-runs (alist-get 'elements paragraph) markers literal-whitespace)))

(defun gdocs-convert-paragraph-from-docs (paragraph runs markers start end style extra)
  "Build native PARAGRAPH IR from RUNS with caller-owned STYLE and EXTRA.
MARKERS, START and END identify document annotations.  EXTRA is an ordered
attribute plist; callers supply role metadata without changing this codec."
  (let ((alignment (gdocs-convert--docs-alignment-to-ir
                    (alist-get 'alignment (alist-get 'paragraphStyle paragraph)))))
    (append (list :type 'paragraph :style style :contents runs
                  :id (gdocs-convert--next-id))
            extra
            (when alignment (list :alignment alignment))
            (when-let* ((annotations (gdocs-convert--paragraph-markers-for-range markers start end)))
              (list :gdocs-marker annotations)))))

(defun gdocs-convert--docs-alignment-to-ir (alignment)
  "Convert a Google Docs ALIGNMENT string to an IR alignment symbol."
  (pcase alignment
    ("CENTER" 'center)
    ("END" 'end)
    ("JUSTIFIED" 'justified)
    (_ nil)))

(defun gdocs-convert-paragraph-to-org (element &optional renderers)
  "Render paragraph ELEMENT using optional ordered feature RENDERERS.
Each renderer receives ELEMENT and decoded inline text, returning a string or
nil when unhandled.  Plain paragraphs return their text unchanged."
  (let ((text (gdocs-convert--runs-to-org (plist-get element :contents))))
    (or (seq-some (lambda (renderer) (funcall renderer element text)) renderers) text)))

(provide 'gdocs-convert-paragraph-codec)
;;; gdocs-convert-paragraph-codec.el ends here
