;;; gdocs-link-resolve.el --- Document-link lookup and anchor caches -*- lexical-binding: t; -*-

;;; Commentary:
;; Resolve document links from explicit caller context and local metadata.
;; This module owns filesystem/ID lookup and heading caches, not conversion,
;; presentation, account selection or transport.  Loading it does not scan notes.
;; Established names and dynamic context bindings remain available to callers.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'seq)
(require 'subr-x)
(declare-function org-id-find "org-id" (id &optional markerp))

(defcustom gdocs-link-directories nil
  "Additional directories to scan for cross-document link resolution.
`gdocs-directory' is always included."
  :type '(repeat directory) :group 'gdocs)

(defvar gdocs-convert--link-context nil
  "Plist with explicit :buffer-file and :docid-map for document link resolution.
Bound by callers for both source and native conversion, including callbacks.")
(defvar gdocs-convert--document-id nil
  "Current native document ID, used to expand internal heading links.")
(defvar gdocs-convert--heading-cache (make-hash-table :test 'equal)
  "Session map from document ID to (heading-text . heading-id) entries.")
(defvar gdocs-convert--heading-reverse-cache (make-hash-table :test 'equal)
  "Session map from document ID to (heading-id . heading-text) entries.")

(defconst gdocs-convert--docs-url-regexp
  "https://docs\\.google\\.com/document/d/\\([^/#]+\\)\\(?:/[^#]*\\)?\\(?:#heading=h\\.\\([^&]+\\)\\)?"
  "Document URL pattern with document ID and optional heading ID groups.")

(defun gdocs-link-resolve--trim (text)
  "Trim TEXT without changing caller match data."
  (save-match-data (string-trim text)))

(defun gdocs-convert--read-file-property-gdocs-id (file-path)
  "Read the file-level GDOCS_DOCUMENT_ID from the first kilobyte of FILE-PATH."
  (when (and file-path (file-readable-p file-path))
    (let ((head-bytes 1024))
      (with-temp-buffer
        (insert-file-contents file-path nil 0
                              (min head-bytes (file-attribute-size (file-attributes file-path))))
        (goto-char (point-min))
        (when (and (looking-at-p ":PROPERTIES:")
                   (re-search-forward "^:GDOCS_DOCUMENT_ID:\\s-+\\(.+\\)$" nil t))
          (string-trim (match-string 1)))))))

(defun gdocs-convert--build-docid-to-file-map (directories)
  "Scan immediate Org files in explicit DIRECTORIES into a document-ID map."
  (let ((map (make-hash-table :test 'equal)))
    (dolist (directory directories)
      (when (file-directory-p directory)
        (dolist (file (directory-files directory t "\\.org\\'"))
          (when-let* ((id (gdocs-convert--read-file-property-gdocs-id file)))
            (puthash id file map)))))
    map))

(defun gdocs-convert--cache-heading-ids (doc-id json)
  "Index heading anchors from native JSON in both directions under DOC-ID."
  (let ((body (alist-get 'body json)) forward reverse)
    (when body
      (seq-doseq (element (alist-get 'content body))
        (when-let* ((paragraph (alist-get 'paragraph element)))
          (let* ((style (alist-get 'paragraphStyle paragraph))
                 (named-style (alist-get 'namedStyleType style))
                 (id (alist-get 'headingId style)))
            (when (and id (string-match-p "^HEADING_[1-6]$" named-style))
              (let ((text (gdocs-convert--docs-paragraph-plain-text (alist-get 'elements paragraph))))
                (when (and text (not (string-empty-p text)))
                  (push (cons text id) forward) (push (cons id text) reverse))))))))
    (puthash doc-id (nreverse forward) gdocs-convert--heading-cache)
    (puthash doc-id (nreverse reverse) gdocs-convert--heading-reverse-cache)))

(defun gdocs-convert--docs-paragraph-plain-text (elements)
  "Concatenate native text ELEMENTS and trim trailing whitespace."
  (let (parts)
    (seq-doseq (element elements)
      (when-let* ((text (alist-get 'textRun element))) (push (alist-get 'content text) parts)))
    (save-match-data (string-trim-right (apply #'concat (nreverse parts))))))

(defun gdocs-convert--link-url (link)
  "Resolve Org LINK using the current explicit document context when available."
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link))
        (search (org-element-property :search-option link)))
    (cond
     ((member type '("http" "https" "ftp" "mailto")) (concat type ":" path))
     ((and gdocs-convert--link-context (string= type "file")) (gdocs-convert--resolve-file-link path search))
     ((and gdocs-convert--link-context (string= type "id")) (gdocs-convert--resolve-id-link path))
     ((string= type "fuzzy")
      (if (and gdocs-convert--link-context (string-prefix-p "*" path))
          (gdocs-convert--resolve-same-doc-heading-link (substring path 1)) path))
     ((string= type "file") (concat "file:" path))
     ((string= type "id") (concat "id:" path))
     (t path))))

(defun gdocs-convert--resolve-file-link (path search)
  "Resolve file PATH and heading SEARCH, leaving unpublished Org targets plain."
  (let* ((source (plist-get gdocs-convert--link-context :buffer-file))
         (absolute (if (file-name-absolute-p path) path (expand-file-name path (file-name-directory source))))
         (doc-id (gdocs-convert--read-file-property-gdocs-id absolute))
         (heading (when (and search (string-prefix-p "*" search)) (substring search 1))))
    (cond (doc-id (gdocs-convert--make-docs-url doc-id heading))
          ((string-suffix-p ".org" path t) nil)
          (t (concat "file:" path)))))

(defun gdocs-convert--resolve-id-link (id)
  "Resolve cached Org ID without triggering a broad ID-location rescan."
  (require 'org-id)
  (when-let* ((location
               (let ((mode-function (symbol-function 'org-mode)))
                 ;; Org-ID switches buffers internally, so delay hooks inside
                 ;; the target buffer rather than binding them in the caller.
                 (cl-letf (((symbol-function 'org-id-update-id-locations) #'ignore)
                           ((symbol-function 'org-mode)
                            (lambda ()
                              (let ((org-inhibit-startup t))
                                (delay-mode-hooks (funcall mode-function))))))
                   (org-id-find id)))))
    (let* ((file (if (consp location) (car location) location))
           (doc-id (gdocs-convert--read-file-property-gdocs-id file)))
      (when doc-id
        (let* ((position (when (consp location) (cdr location)))
               (heading (when position (gdocs-convert--heading-at-pos file position))))
          (gdocs-convert--make-docs-url doc-id heading))))))

(defun gdocs-convert--resolve-same-doc-heading-link (heading-text)
  "Resolve HEADING-TEXT in the current source document or retain its fuzzy link."
  (let* ((file (plist-get gdocs-convert--link-context :buffer-file))
         (id (when file (gdocs-convert--read-file-property-gdocs-id file))))
    (if id (gdocs-convert--make-docs-url id heading-text) (concat "*" heading-text))))

(defun gdocs-convert--heading-at-pos (file pos)
  "Read heading text at POS in FILE without entering a user editing session."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((org-inhibit-startup t)) (delay-mode-hooks (org-mode)))
    (goto-char pos)
    (when (org-at-heading-p) (org-element-property :raw-value (org-element-at-point)))))

(defun gdocs-convert--make-docs-url (doc-id &optional heading-search)
  "Build DOC-ID's URL with an optional cached HEADING-SEARCH anchor."
  (let ((base (format "https://docs.google.com/document/d/%s/edit" doc-id)))
    (if heading-search
        (let* ((entries (gethash doc-id gdocs-convert--heading-cache))
               (normalized (gdocs-link-resolve--trim (downcase heading-search)))
               (id (cl-loop for (text . id) in entries
                            when (string= (gdocs-link-resolve--trim (downcase text)) normalized) return id)))
          (if id (concat base "#heading=h." id) base)) base)))

(defun gdocs-convert--inline-link-for-org (link)
  "Resolve LINK back to Org when an explicit link context is bound."
  (if gdocs-convert--link-context (gdocs-convert--reverse-resolve-link link) link))

(defun gdocs-convert--reverse-resolve-link (url)
  "Resolve known document URL back to an Org file/ID link, retaining unknown URLs."
  (if (string-match gdocs-convert--docs-url-regexp url)
      (let* ((doc-id (match-string 1 url)) (heading-id (match-string 2 url))
             (map (plist-get gdocs-convert--link-context :docid-map))
             (source (plist-get gdocs-convert--link-context :buffer-file))
             (target (when map (gethash doc-id map))))
        (if target
            (gdocs-convert--format-org-link target source
                                            (gdocs-convert--lookup-heading-text doc-id heading-id)
                                            (gdocs-convert--same-file-p target source)) url)) url))

(defun gdocs-convert--lookup-heading-text (doc-id heading-id)
  "Look up HEADING-ID text in DOC-ID's reverse cache."
  (when heading-id (cdr (assoc heading-id (gethash doc-id gdocs-convert--heading-reverse-cache)))))

(defun gdocs-convert--same-file-p (file-a file-b)
  "Return non-nil if FILE-A and FILE-B have the same expanded path."
  (and file-a file-b (string= (expand-file-name file-a) (expand-file-name file-b))))

(defun gdocs-convert--format-org-link (target-file buffer-file heading-text same-doc-p)
  "Format TARGET-FILE relative to BUFFER-FILE with HEADING-TEXT and SAME-DOC-P."
  (cond ((and same-doc-p heading-text) (gdocs-convert--same-doc-heading-link buffer-file heading-text))
        (heading-text (format "file:%s::*%s" (file-relative-name target-file (file-name-directory buffer-file)) heading-text))
        (t (concat "file:" (file-relative-name target-file (file-name-directory buffer-file))))))

(defun gdocs-convert--same-doc-heading-link (file heading-text)
  "Prefer a local Org ID for HEADING-TEXT in FILE, falling back to its fuzzy link."
  (if-let* ((id (gdocs-convert--find-heading-org-id file heading-text)))
      (concat "id:" id) (concat "*" heading-text)))

(defun gdocs-convert--find-heading-org-id (file heading-text)
  "Read HEADING-TEXT's Org ID from FILE without user hooks or startup actions."
  (when (and file heading-text (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((org-inhibit-startup t)) (delay-mode-hooks (org-mode)))
      (goto-char (point-min))
      (let ((normalized (gdocs-link-resolve--trim (downcase heading-text))))
        (catch 'found
          (while (re-search-forward org-heading-regexp nil t)
            (when (string= (gdocs-link-resolve--trim (downcase (org-get-heading t t t t))) normalized)
              (throw 'found (org-entry-get nil "ID")))))))))

(defun gdocs-convert--extract-link-url (link-obj)
  "Extract LINK-OBJ's full URL or expand its native internal heading ID."
  (or (alist-get 'url link-obj) (gdocs-convert--heading-id-to-url (alist-get 'headingId link-obj))))

(defun gdocs-convert--heading-id-to-url (heading-id)
  "Expand HEADING-ID using the explicit current native document ID."
  (when (and heading-id gdocs-convert--document-id)
    (format "https://docs.google.com/document/d/%s/edit#heading=h.%s" gdocs-convert--document-id heading-id)))

(provide 'gdocs-link-resolve)
;;; gdocs-link-resolve.el ends here
