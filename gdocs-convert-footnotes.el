;;; gdocs-convert-footnotes.el --- Native footnote conversion -*- lexical-binding: t; -*-

;;; Commentary:
;; Org definitions, native segment contents and segment-local requests share the
;; ordinary text-run IR.  Transport and asynchronous createFootnote choreography
;; belong to callers, not to semantic conversion.

;;; Code:

(require 'org-element)
(require 'seq)
(require 'subr-x)

(defvar gdocs-convert-footnote-reference-text-function)
(declare-function gdocs-convert--objects-to-runs "gdocs-convert" (objects))
(declare-function gdocs-convert--trim-runs "gdocs-convert" (runs))
(declare-function gdocs-convert--make-plain-run "gdocs-convert" (text))
(declare-function gdocs-convert--next-id "gdocs-convert" ())
(declare-function gdocs-convert--runs-to-org "gdocs-convert" (runs))
(declare-function gdocs-convert--runs-to-plain-text "gdocs-convert" (runs))
(declare-function gdocs-convert--docs-elements-to-runs "gdocs-convert" (elements &optional markers))
(declare-function gdocs-convert--trim-trailing-newline-runs "gdocs-convert" (runs))
(declare-function gdocs-convert--string-to-utf16-length "gdocs-convert" (string))
(declare-function gdocs-convert--text-range "gdocs-convert" (start end &optional segment-id))
(declare-function gdocs-convert--make-text-style-reset-request "gdocs-convert" (start end))
(declare-function gdocs-convert-make-text-style-requests "gdocs-convert"
                  (runs start-index &optional segment-id omit-status-markers))
(declare-function gdocs-convert--make-insert-text-request "gdocs-convert" (text index))
(declare-function gdocs-convert--make-marker-requests "gdocs-convert" (element start end))

(defun gdocs-convert--footnote-definition-to-ir (footnote-def)
  "Convert a FOOTNOTE-DEF element to an IR element."
  (let* ((label (org-element-property :label footnote-def))
         (runs (gdocs-convert--footnote-body-runs footnote-def)))
    (list :type 'footnote :label label
          :contents (or runs (list (gdocs-convert--make-plain-run "")))
          :id (gdocs-convert--next-id))))

(defun gdocs-convert--footnote-body-runs (footnote-def)
  "Extract text runs from the body of FOOTNOTE-DEF."
  (let (all-runs)
    (org-element-map footnote-def 'paragraph
                     (lambda (para)
                       (setq all-runs (nconc all-runs
                                             (gdocs-convert--objects-to-runs
                                              (org-element-contents para))))))
    (gdocs-convert--trim-runs all-runs)))

(defun gdocs-convert-footnote-body-runs (definition)
  "Convert a single-paragraph Org footnote DEFINITION into rich IR runs.
Reject unsupported segment content before callers perform remote mutations."
  (let ((paragraphs (org-element-map definition 'paragraph #'identity)))
    (unless (and (= (length paragraphs) 1)
                 (seq-every-p (lambda (element)
                                (memq (org-element-type element) '(paragraph property-drawer)))
                              (org-element-contents definition)))
      (user-error "Native footnotes currently require a single paragraph"))
    (let ((runs (gdocs-convert--footnote-body-runs definition)))
      (when (seq-some (lambda (run)
                        (or (plist-get run :date-element)
                            (plist-get run :footnote-label)
                            (plist-get run :status)
                            (plist-get run :superscript)
                            (plist-get run :subscript)
                            (string-match-p "\n" (or (plist-get run :text) "")))) runs)
        (user-error "Native footnotes cannot contain dates, nested notes, status labels, baseline shifts or paragraph breaks"))
      runs)))

(defun gdocs-convert-footnote-label-requests (label index)
  "Preserve authored LABEL on a native footnote reference at body INDEX."
  (gdocs-convert--make-marker-requests
   (list :id (gdocs-convert--next-id)
         :gdocs-marker (list (list :type 'footnote-label :data label)))
   index (+ index 2)))

(defun gdocs-convert-footnote-body-requests (runs segment-id &optional start-index)
  "Return text insertion and rich styling for RUNS in native SEGMENT-ID.
START-INDEX defaults to one, after Google's native footnote prefix."
  (let ((start (or start-index 1)))
    (append
     (list `((insertText . ((text . ,(gdocs-convert--runs-to-plain-text runs))
                            (location . ((segmentId . ,segment-id) (index . ,start)))))))
     (gdocs-convert-make-text-style-requests runs start segment-id))))

(defun gdocs-convert-footnote-update-requests (old new)
  "Return segment-only requests replacing OLD footnote contents with NEW.
Text-identical changes preserve the native text and reset only its styling."
  (let* ((segment (plist-get old :segment-id))
         (start (or (plist-get old :segment-start) 1))
         (old-text (gdocs-convert--runs-to-plain-text (plist-get old :contents)))
         (runs (plist-get new :contents))
         (new-text (gdocs-convert--runs-to-plain-text runs))
         (end (+ start (gdocs-convert--string-to-utf16-length new-text)))
         (reset (and (> end start)
                     (gdocs-convert--make-text-style-reset-request start end))))
    (unless segment (user-error "Missing native footnote segment identity"))
    (when reset
      (let ((update (alist-get 'updateTextStyle reset)))
        (setf (alist-get 'range update) (gdocs-convert--text-range start end segment)
              (alist-get 'fields update) (concat (alist-get 'fields update) ",link"))))
    (append
     (unless (equal old-text new-text)
       (append
        (when (> (length old-text) 0)
          (list `((deleteContentRange
                   . ((range . ,(gdocs-convert--text-range
                                 start (+ start (gdocs-convert--string-to-utf16-length old-text)) segment)))))))
        (list `((insertText . ((text . ,new-text)
                               (location . ((segmentId . ,segment) (index . ,start)))))))))
     (when reset (list reset))
     (gdocs-convert-make-text-style-requests runs start segment))))

(defun gdocs-convert--ir-footnote-to-org (element)
  "Convert an IR footnote ELEMENT to an Org footnote definition string."
  (let ((label (plist-get element :label))
        (text (gdocs-convert--runs-to-org (plist-get element :contents))))
    (format "[fn:%s] %s" (or label "") text)))

(defun gdocs-convert--trim-leading-footnote-space-runs (runs)
  "Return RUNS with Google's leading native-footnote space removed."
  (if-let* ((first (car runs))
            (text (plist-get first :text))
            ((string-match-p "\\`[[:space:]]+" text)))
      (let* ((trimmed-first (copy-sequence first))
             (trimmed-text (string-trim-left text)))
        (plist-put trimmed-first :text trimmed-text)
        (if (string-empty-p trimmed-text) (cdr runs)
          (cons trimmed-first (cdr runs))))
    runs))

(defun gdocs-convert--docs-footnote-content-to-runs (content)
  "Convert Google Docs footnote CONTENT to IR text runs.
Reject structures that the single-paragraph native mapping cannot preserve."
  (unless (and (= (length content) 1)
               (let ((paragraph (alist-get 'paragraph (elt content 0))))
                 (and paragraph
                      (seq-every-p (lambda (part) (alist-get 'textRun part))
                                   (alist-get 'elements paragraph)))))
    (user-error "Only single-paragraph native footnotes containing text runs are supported"))
  (let (runs first-paragraph-seen)
    (seq-doseq (element content)
      (when-let* ((paragraph (alist-get 'paragraph element)))
        (when first-paragraph-seen
          (push (gdocs-convert--make-plain-run "\n") runs))
        (setq first-paragraph-seen t
              runs (append (nreverse
                            (gdocs-convert--trim-trailing-newline-runs
                             (gdocs-convert--docs-elements-to-runs
                              (alist-get 'elements paragraph))))
                           runs))))
    (gdocs-convert--trim-leading-footnote-space-runs (nreverse runs))))

(defun gdocs-convert--footnote-segment-start (content)
  "Return the first authored UTF-16 index in footnote CONTENT."
  (let* ((entry (seq-find (lambda (element) (alist-get 'paragraph element)) content))
         (part (car (append (alist-get 'elements (alist-get 'paragraph entry)) nil)))
         (text (or (alist-get 'content (alist-get 'textRun part)) ""))
         (index (or (alist-get 'startIndex part) (alist-get 'startIndex entry) 0)))
    (+ index (if (string-match "\\`[[:space:]]+" text)
                 (gdocs-convert--string-to-utf16-length (match-string 0 text)) 0))))

(defun gdocs-convert--docs-footnotes-to-ir (footnotes footnote-refs)
  "Convert Google Docs FOOTNOTES encountered in FOOTNOTE-REFS to IR."
  (let ((seen (make-hash-table :test 'equal)) result)
    (dolist (ref footnote-refs)
      (let* ((footnote-id (plist-get ref :id))
             (label (plist-get ref :label))
             (footnote (and footnotes (alist-get (intern footnote-id) footnotes))))
        (unless (or (gethash footnote-id seen) (not footnote))
          (puthash footnote-id t seen)
          (push (list :type 'footnote :label label :segment-id footnote-id
                      :segment-start (gdocs-convert--footnote-segment-start
                                      (alist-get 'content footnote))
                      :contents (gdocs-convert--docs-footnote-content-to-runs
                                 (alist-get 'content footnote))
                      :id (gdocs-convert--next-id)) result))))
    (nreverse result)))

(defun gdocs-convert--footnote-to-requests (element index)
  "Render ELEMENT at INDEX, omitting native definitions from the body."
  (if gdocs-convert-footnote-reference-text-function
      (list :requests nil :index index)
    (let* ((text (gdocs-convert--runs-to-plain-text (plist-get element :contents)))
           (full-text (concat "[fn:" (or (plist-get element :label) "") "] " text "\n")))
      (list :requests (list (gdocs-convert--make-insert-text-request full-text index))
            :index (+ index (gdocs-convert--string-to-utf16-length full-text))))))

(provide 'gdocs-convert-footnotes)
;;; gdocs-convert-footnotes.el ends here
