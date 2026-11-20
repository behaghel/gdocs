;;; gdocs-diff-callouts.el --- Callout identity and native geometry -*- lexical-binding: t; -*-

;;; Commentary:
;; Callout type/title, list state, rich body and generated label/separator
;; geometry belong here rather than generic LCS and index algorithms.

;;; Code:

(require 'gdocs-convert-callouts)
(defvar gdocs-diff-semantic-element-functions)
(declare-function gdocs-diff--runs-formatting-key "gdocs-diff" (runs))
(declare-function gdocs-diff--runs-semantic-text "gdocs-diff" (runs))
(declare-function gdocs-diff--list-key "gdocs-diff" (list-info))

(defun gdocs-diff--callout-block-key (element)
  "Return callout ELEMENT semantic identity, excluding transient list counters."
  (format "callout-block:%s:%s:%s:%s"
          (or (plist-get element :callout-type) "info") (or (plist-get element :title) "")
          (or (plist-get element :style-revision) "")
          (mapconcat (lambda (body)
                       (let ((runs (plist-get body :contents)))
                         (format "%s:%s:%s" (or (gdocs-convert-list-continuation-key body)
                                                (gdocs-diff--list-key (plist-get body :list)))
                                 (gdocs-diff--runs-formatting-key runs) (gdocs-diff--runs-semantic-text runs))))
                     (gdocs-convert--callout-body-elements-from-paragraphs element) "\n\n")))

(defun gdocs-diff--callout-block-utf16-length (element)
  "Return callout ELEMENT body length including visible label and separator."
  (let* ((type (or (plist-get element :callout-type) "info")) (title (plist-get element :title))
         (label (1+ (gdocs-convert--string-to-utf16-length (gdocs-convert--callout-label type title))))
         (body (apply #'+ (mapcar (lambda (paragraph)
                                    (let ((physical (gdocs-convert-task-project-paragraph paragraph)))
                                      (1+ (gdocs-convert--string-to-utf16-length
                                           (gdocs-convert--runs-to-plain-text (plist-get physical :contents))))))
                                  (gdocs-convert--callout-body-elements-from-paragraphs element)))))
    (+ label body 1)))

(defun gdocs-diff-callouts--element (element)
  "Describe callout ELEMENT, or decline other kinds."
  (when (eq (plist-get element :type) 'callout-block)
    (list :key (gdocs-diff--callout-block-key element) :body-length (gdocs-diff--callout-block-utf16-length element))))

(add-hook 'gdocs-diff-semantic-element-functions #'gdocs-diff-callouts--element t)

(provide 'gdocs-diff-callouts)
;;; gdocs-diff-callouts.el ends here
