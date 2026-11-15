;;; gdocs-convert-paragraphs.el --- Composable paragraph primitives -*- lexical-binding: t; -*-

;;; Commentary:
;; Paragraph construction and literal text insertion compose the text primitives.
;; Callers own semantic recognition, prefixes, IDs and configurable styling.
;; A literal paragraph includes exactly one additional terminal newline; its
;; length is measured in the Docs UTF-16 model, not Emacs character positions.

;;; Code:

(require 'gdocs-convert-text)

(defun gdocs-convert-make-paragraph (contents &rest props)
  "Create reusable semantic paragraph IR with CONTENTS and PROPS.
PROPS may include `:style', `:style-name', `:list', `:gdocs-marker', and `:id'."
  (append (list :type 'paragraph
                :style (or (plist-get props :style) 'normal)
                :contents contents)
          (when (plist-member props :style-name)
            (list :style-name (plist-get props :style-name)))
          (when (plist-member props :list)
            (list :list (plist-get props :list)))
          (when (plist-member props :gdocs-marker)
            (list :gdocs-marker (plist-get props :gdocs-marker)))
          (when (plist-member props :id)
            (list :id (plist-get props :id)))))

(defun gdocs-convert-paragraph-text-requests (text index &optional prefix)
  "Plan literal TEXT with PREFIX and a paragraph newline at INDEX.
Return :requests and UTF-16 :length.  TEXT and PREFIX are already rendered
strings, not semantic runs.  Styles and feature-specific requests are composed
by callers without changing this literal envelope."
  (let ((full-text (concat prefix text "\n")))
    (list :requests (list (gdocs-convert--make-insert-text-request full-text index))
          :length (gdocs-convert--string-to-utf16-length full-text))))

(provide 'gdocs-convert-paragraphs)
;;; gdocs-convert-paragraphs.el ends here
