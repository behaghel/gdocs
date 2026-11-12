;;; gdocs-convert-status.el --- Static status label conversion -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pablo Stafforini

;;; Commentary:
;; Status labels carry colour identifiers and plain text, not dropdown options.
;; Presentation defaults live in gdocs-styles.  Shared run and index helpers
;; are supplied by gdocs-convert, without a circular require on the dispatcher.

;;; Code:

(require 'cl-lib)
(require 'org-element)
(require 'seq)
(require 's)
(require 'subr-x)
(require 'gdocs-styles)

(declare-function gdocs-convert--logical-text-style-to-docs "gdocs-convert" (style-plist))
(declare-function gdocs-convert--text-range "gdocs-convert" (start end &optional segment-id))
(declare-function gdocs-convert--make-plain-run "gdocs-convert" (text))
(declare-function gdocs-convert--docs-text-run-to-ir "gdocs-convert" (text-run))
(declare-function gdocs-convert--string-to-utf16-length "gdocs-convert" (string))

(defun gdocs-convert--status-docs-style (status)
  "Return the Docs text style for semantic STATUS."
  (let* ((key (gdocs-convert--status-color-key (plist-get status :color)))
         (definition (assq (intern (concat "gdocs-status-" key)) gdocs-style-definitions)))
    (gdocs-convert--logical-text-style-to-docs
     (if definition (plist-get (cdr definition) :text)
       (cdr (assq (intern key) gdocs-status-default-styles))))))

(defun gdocs-convert--status-marker-request (status start end &optional segment-id)
  "Return the semantic STATUS marker covering START to END.
SEGMENT-ID must be nil; status labels inside footnotes are unsupported."
  (when segment-id (user-error "Status labels inside document segments are unsupported"))
  `((createNamedRange
     . ((name . ,(format "gdocs-org-marker:status:run-%d:%s:%s:%d"
                         start (plist-get status :link-type) (plist-get status :color)
                         (if (plist-get status :described) 1 0)))
        (range . ,(gdocs-convert--text-range start end))))))

(defun gdocs-convert--status-color-key (color)
  "Return the supported logical status key for COLOR, or signal an error."
  (let ((key (downcase (or color ""))))
    (when (equal key "gray") (setq key "grey"))
    (unless (member key '("grey" "red" "yellow" "green" "blue" "purple"))
      (user-error "Unsupported status colour `%s'" color))
    key))

(defun gdocs-convert--status-link-to-run (link inherited-props)
  "Convert a static status LINK to a semantic run, or return nil.
Reject rich labels and unsupported contexts instead of making a colour URL.
INHERITED-PROPS must not add invisible emphasis to the generated pill style."
  (let ((raw (org-element-property :raw-link link))
        (case-fold-search t))
    (when (and raw (string-match "\\`\\(status\\|confluence-status\\):\\(.*\\)\\'" raw))
      (let* ((protocol (match-string 1 raw))
             (color (match-string 2 raw))
             (children (org-element-contents link))
             (label (substring-no-properties
                     (if children (apply #'concat (seq-filter #'stringp children)) color)))
             (container (org-element-lineage link '(paragraph headline))))
        (gdocs-convert--status-color-key color)
        (when (or (not (seq-every-p #'stringp children))
                  (seq-some (lambda (key) (plist-get inherited-props key))
                            '(:bold :italic :underline :strikethrough :code :subscript :superscript))
                  (org-element-lineage link '(table footnote-definition footnote-reference)))
          (user-error "Status labels support plain body/list/heading text only; no rich label, table, or footnote"))
        (when (and container
                   (org-element-map
                    (if (eq (org-element-type container) 'headline)
                        (org-element-property :title container) container)
                    '(timestamp footnote-reference) #'identity nil t))
          (user-error "Status labels mixed with native dates or footnotes in one paragraph are unsupported"))
        (when (or (string-blank-p label) (string-match-p "[\n\r]" label))
          (user-error "A status label must be nonempty and stay in one paragraph"))
        (plist-put (gdocs-convert--make-plain-run label) :status
                   (list :link-type protocol :color color :described (not (null children))))))))

(defun gdocs-convert--status-run-to-org (run)
  "Render semantic status RUN using its original link spelling and label."
  (let* ((status (plist-get run :status))
         (target (format "%s:%s" (plist-get status :link-type) (plist-get status :color)))
         (text (plist-get run :text)))
    (when (or (not (stringp text)) (string-blank-p text)
              (string-match-p "[\n\r]\\|\\]\\]" text))
      (user-error "Status label cannot be represented as a single plain Org link description"))
    (if (or (plist-get status :described)
            (not (equal text (plist-get status :color))))
        (format "[[%s][%s]]" target text)
      (format "[[%s]]" target))))

(defun gdocs-convert--status-marker-data (marker)
  "Decode and validate semantic status MARKER data."
  (let* ((parts (s-split ":" (or (plist-get marker :data) "")))
         (protocol (nth 0 parts)) (color (nth 1 parts)) (described (nth 2 parts)))
    (unless (and (= (length parts) 3)
                 (member (downcase protocol) '("status" "confluence-status"))
                 (member described '("0" "1")))
      (user-error "Invalid status marker metadata"))
    (gdocs-convert--status-color-key color)
    (list :link-type protocol :color color :described (equal described "1"))))

(defun gdocs-convert--utf16-string-index (text units)
  "Return the character index in TEXT at exactly UNITS UTF-16 code units."
  (let ((index 0) (offset 0))
    (while (and (< offset units) (< index (length text)))
      (setq offset (+ offset (if (> (aref text index) #xFFFF) 2 1))
            index (1+ index)))
    (unless (= offset units)
      (user-error "Status marker splits a Unicode character or exceeds its text"))
    index))

(defun gdocs-convert--docs-status-text-parts (element markers)
  "Split text ELEMENT at semantic status MARKERS into (MARKER . RUN) pairs."
  (let* ((run (gdocs-convert--docs-text-run-to-ir (alist-get 'textRun element)))
         (text (plist-get run :text))
         (start (or (alist-get 'startIndex element) 0))
         (end (+ start (gdocs-convert--string-to-utf16-length text)))
         (statuses (seq-filter
                    (lambda (marker)
                      (and (eq (plist-get marker :type) 'status)
                           (< (plist-get marker :start) end)
                           (> (plist-get marker :end) start))) markers)))
    (if (null statuses)
        (list (cons nil run))
      (let* ((cuts (sort (delete-dups
                          (append (list start end)
                                  (cl-mapcan (lambda (marker)
                                               (list (max start (plist-get marker :start))
                                                     (min end (plist-get marker :end)))) statuses))) #'<))
             (parts nil))
        (while (cdr cuts)
          (let* ((a (car cuts)) (b (cadr cuts))
                 (covering (seq-filter (lambda (marker)
                                         (and (<= (plist-get marker :start) a)
                                              (>= (plist-get marker :end) b))) statuses))
                 (marker (car covering))
                 (value (substring text
                                   (gdocs-convert--utf16-string-index text (- a start))
                                   (gdocs-convert--utf16-string-index text (- b start))))
                 (part (if marker (gdocs-convert--make-plain-run value)
                         (plist-put (copy-sequence run) :text value))))
            (when (> (length covering) 1)
              (user-error "Overlapping status markers require manual review"))
            (when marker
              (when (string-match-p "[\n\r]" value)
                (user-error "A status marker cannot cross paragraph boundaries"))
              (plist-put part :status (gdocs-convert--status-marker-data marker))
              (plist-put part :status-marker-ids (list (plist-get marker :range-id))))
            (push (cons marker part) parts))
          (setq cuts (cdr cuts)))
        (nreverse parts)))))

(provide 'gdocs-convert-status)
;;; gdocs-convert-status.el ends here
