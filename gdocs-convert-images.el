;;; gdocs-convert-images.el --- Source image paragraph recognition -*- lexical-binding: t; -*-

;;; Commentary:
;; Image syntax is recognized before generic inline conversion.  Transport,
;; uploading and document orchestration are not responsibilities of this module.

;;; Code:

(require 'org-element)
(require 'seq)
(require 's)
(require 'subr-x)

(declare-function gdocs-convert--next-id "gdocs-convert" ())

(defconst gdocs-convert-image-file-extensions
  '("png" "jpg" "jpeg" "gif" "webp" "svg")
  "Local image file extensions that can be represented as image IR.")

(defun gdocs-convert-image-paragraph-to-ir (paragraph)
  "Return an image element list for Org PARAGRAPH, or nil when unhandled."
  (when-let* ((image (gdocs-convert--standalone-image-paragraph-to-ir paragraph)))
    (list image)))

(defun gdocs-convert--standalone-image-paragraph-to-ir (paragraph)
  "Return image IR when PARAGRAPH is a standalone local image link."
  (let ((objects (seq-filter (lambda (object)
                               (not (and (stringp object) (string-blank-p object))))
                             (org-element-contents paragraph))))
    (when (and (= (length objects) 1)
               (gdocs-convert--standalone-image-link-p (car objects)))
      (let* ((link (car objects))
             (path (org-element-property :path link))
             (caption (gdocs-convert--paragraph-caption paragraph)))
        (append (list :type 'image :path path :id (gdocs-convert--next-id))
                (when caption (list :caption caption)))))))

(defun gdocs-convert--standalone-image-link-p (object)
  "Return non-nil when OBJECT is an undescribed local image link."
  (and (not (stringp object))
       (eq (org-element-type object) 'link)
       (string= (org-element-property :type object) "file")
       (null (org-element-contents object))
       (member (downcase (or (file-name-extension (org-element-property :path object)) ""))
               gdocs-convert-image-file-extensions)))

(defun gdocs-convert--paragraph-caption (paragraph)
  "Return Org caption text affiliated with PARAGRAPH, or nil."
  (when-let* ((caption (org-element-property :caption paragraph)))
    (s-trim (org-element-interpret-data caption))))

(provide 'gdocs-convert-images)
;;; gdocs-convert-images.el ends here
