;;; gdocs-convert-images.el --- Image conversion and request planning -*- lexical-binding: t; -*-

;;; Commentary:
;; Own image recognition, native decoding, source/caption metadata, Org
;; rendering and request geometry.  Shared text and logical-style services
;; supply literal text/ranges and presentation.  Transport, uploads and
;; asynchronous document orchestration are not responsibilities of this module.

;;; Code:

(require 'org-element)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'gdocs-convert-text)
(require 'gdocs-style-resolve)

(declare-function gdocs-convert--next-id "gdocs-convert" ())
(declare-function gdocs-convert--markers-for-range "gdocs-convert" (markers start end))

(defcustom gdocs-image-caption-style-name "org-image-caption"
  "Semantic style name recorded in Org image caption markers.
Visual styling is resolved separately through logical styles."
  :type 'string
  :group 'gdocs)

(defun gdocs-convert-image-element-p (element)
  "Return non-nil when ELEMENT is image IR."
  (eq (plist-get element :type) 'image))

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
    (save-match-data (string-trim (org-element-interpret-data caption)))))

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
               (gdocs-convert-image-element-p (car result)))
          (let ((image (copy-sequence (car result)))
                (caption (gdocs-convert--runs-to-plain-text
                          (plist-get element :contents))))
            (plist-put image :caption caption)
            (when-let* ((caption-end (plist-get element :doc-end)))
              (plist-put image :doc-end caption-end))
            (setq result (cons image (cdr result))))
        (push element result)))
    (nreverse result)))

(defun gdocs-convert-image-native-element-to-ir (element inline-objects markers start end)
  "Decode native inline image ELEMENT or return nil when unhandled.
INLINE-OBJECTS supplies metadata; MARKERS overlapping START to END supply
source ownership.  Document traversal preserves ordering with other features."
  (when-let* ((object (alist-get 'inlineObjectElement element)))
    (gdocs-convert--docs-inline-object-to-image-ir
     (alist-get 'inlineObjectId object) inline-objects
     (gdocs-convert--markers-for-range markers start end))))

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
                             (decode-coding-string
                              (url-unhex-string
                               (or (plist-get source-marker :data) ""))
                              'utf-8)))
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

(defun gdocs-convert--image-to-requests (element index)
  "Create native image and optional caption requests for ELEMENT at INDEX.
A missing fetchable URI defers publication to adapter preflight."
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

(provide 'gdocs-convert-images)
;;; gdocs-convert-images.el ends here
