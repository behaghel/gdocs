;;; gdocs-diff-images.el --- Image identity and model addressing -*- lexical-binding: t; -*-

;;; Commentary:
;; Image identity prefers authored ownership over transient remote identifiers.
;; Native image body fallback occupies one unit; authoritative document ranges
;; include any attached caption.  Generic diff consumes neutral descriptors.

;;; Code:

(defvar gdocs-diff-semantic-element-functions)

(defun gdocs-diff--image-key (element)
  "Compute a content key for an image ELEMENT.
An authored source path and caption take precedence over remote object IDs
and transient image URIs, keeping create/fetch identity stable."
  (format "image:%s:%s"
          (or (plist-get element :path)
              (plist-get element :source-uri)
              (plist-get element :content-uri)
              (plist-get element :object-id)
              "")
          (or (plist-get element :caption) "")))

(defun gdocs-diff-images--element (element)
  "Describe image ELEMENT or decline other kinds."
  (when (eq (plist-get element :type) 'image)
    (list :key (gdocs-diff--image-key element) :body-length 1)))

(add-hook 'gdocs-diff-semantic-element-functions #'gdocs-diff-images--element t)

(provide 'gdocs-diff-images)
;;; gdocs-diff-images.el ends here
