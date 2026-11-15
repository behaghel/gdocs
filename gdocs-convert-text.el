;;; gdocs-convert-text.el --- Shared text and run primitives -*- lexical-binding: t; -*-

;;; Commentary:
;; Literal text, run construction and UTF-16 coordinates are shared building
;; blocks.  This module does not recognize document features, resolve styles,
;; parse Org, choose accounts or perform transport.  Established function names
;; remain available without loading the conversion facade.

;;; Code:

(require 'seq)
(require 'subr-x)

(defun gdocs-convert-make-run (text &rest props)
  "Create a reusable semantic text run for TEXT with PROPS.
Supported PROPS include inline formatting flags like `:bold', `:italic',
`:underline', `:strikethrough', `:link', and logical `:style-names'."
  (gdocs-convert--make-run text props))

(defun gdocs-convert--make-run (text props)
  "Create a text run plist for TEXT with formatting from PROPS."
  (list :text text
        :bold (plist-get props :bold)
        :italic (plist-get props :italic)
        :underline (plist-get props :underline)
        :strikethrough (plist-get props :strikethrough)
        :code (plist-get props :code)
        :style-names (plist-get props :style-names)
        :link (plist-get props :link)
        :subscript (plist-get props :subscript)
        :superscript (plist-get props :superscript)
        :foreground-color (plist-get props :foreground-color)
        :background-color (plist-get props :background-color)
        :font-size (plist-get props :font-size)))

(defun gdocs-convert--make-plain-run (text)
  "Create a plain (unformatted) text run plist for TEXT."
  (gdocs-convert--make-run text nil))

(defun gdocs-convert--string-to-run (text inherited-props)
  "Create source text runs from TEXT with INHERITED-PROPS.
Normalize soft source wraps to spaces; explicit line-break objects are handled
by the caller.  Literal constructors do not apply this source-only policy."
  (unless (string-empty-p text)
    (list (gdocs-convert--make-run
           (string-replace "\n" " " (substring-no-properties text))
           inherited-props))))

(defun gdocs-convert--strip-vertical-tabs (text)
  "Replace vertical tab characters in TEXT with newlines.
Google Docs uses vertical tabs (^K, U+000B) for soft line breaks
within paragraphs."
  (replace-regexp-in-string "\v" "\n" text))

(defun gdocs-convert--runs-to-plain-text (runs)
  "Concatenate the :text values of all RUNS.
Callers resolve semantic objects before requesting literal text."
  (mapconcat (lambda (run) (plist-get run :text)) runs ""))

(defun gdocs-convert--string-to-utf16-length (string)
  "Return the number of UTF-16 code units needed to encode STRING.
Google Docs uses UTF-16 for indexing.  Characters above U+FFFF
require 2 code units (surrogate pairs), all others require 1."
  (let ((len 0))
    (seq-doseq (char string)
      (setq len (+ len (if (> char #xFFFF) 2 1))))
    len))

(defun gdocs-convert--make-insert-text-request (text index)
  "Create an insertText request for TEXT at INDEX."
  `((insertText . ((text . ,text)
                   (location . ((index . ,index)))))))

(defun gdocs-convert--text-range (start end &optional segment-id)
  "Return a Google Docs text range from START to END.
When SEGMENT-ID is non-nil, target that segment instead of the body."
  (append (when segment-id (list (cons 'segmentId segment-id)))
          (list (cons 'startIndex start) (cons 'endIndex end))))

(defun gdocs-convert--soft-wrap-continuation-p (runs)
  "Return non-nil when RUNS look like a split soft-wrapped paragraph line."
  (let ((text (string-trim-left (gdocs-convert--runs-to-plain-text runs))))
    (and (not (string-empty-p text))
         (let ((first (aref text 0)))
           (or (and (>= first ?a) (<= first ?z))
               (member first '(?, ?. ?\; ?: ?\) ?\] ?\})))))))

(defun gdocs-convert--utf16-string-index (text units)
  "Return the character index in TEXT at exactly UNITS UTF-16 code units."
  (let ((index 0) (offset 0))
    (while (and (< offset units) (< index (length text)))
      (setq offset (+ offset (if (> (aref text index) #xFFFF) 2 1))
            index (1+ index)))
    (unless (= offset units)
      (user-error "Marker splits a Unicode character or exceeds its text"))
    index))

(provide 'gdocs-convert-text)
;;; gdocs-convert-text.el ends here
