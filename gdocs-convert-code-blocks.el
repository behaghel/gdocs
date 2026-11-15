;;; gdocs-convert-code-blocks.el --- Source/example block semantics -*- lexical-binding: t; -*-

;;; Commentary:
;; Own block source syntax, native marker grouping, legacy code grouping,
;; rendering and publication planning.  Shared paragraph services provide
;; construction and request emission; no transport or account handling here.

;;; Code:

(require 'cl-lib)
(require 'org-element)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'url-util)
(require 'gdocs-convert-paragraphs)

(declare-function gdocs-convert--next-id "gdocs-convert" ())
(declare-function gdocs-convert--extract-marker-field "gdocs-convert" (marker type-sym))
(declare-function gdocs-convert--plain-text "gdocs-convert" (element))
(declare-function gdocs-convert--paragraph-to-requests "gdocs-convert" (element index))

(defun gdocs-convert-code-block-element-p (element)
  "Return non-nil for semantic source or example block ELEMENT."
  (memq (plist-get element :type) '(source-block example-block)))

(defun gdocs-convert--code-props (&optional inherited-props)
  "Return code formatting props derived from INHERITED-PROPS."
  (let ((props (copy-sequence inherited-props)))
    (setq props (plist-put props :code t))
    (plist-put props :style-names (cons 'gdocs-code (plist-get props :style-names)))))

(defun gdocs-convert-code--body-lines (value)
  "Split literal block VALUE, removing only its structural final newline."
  (split-string (if (string-suffix-p "\n" value) (substring value 0 -1) value) "\n"))

(defun gdocs-convert--src-block-to-ir (src-block)
  "Convert a SRC-BLOCK to a semantic source-block IR element."
  (let* ((language (or (org-element-property :language src-block) ""))
         (value (or (org-element-property :value src-block) ""))
         (lines (gdocs-convert-code--body-lines value)))
    (list :type 'source-block :language language :lines lines :id (gdocs-convert--next-id))))

(defun gdocs-convert--example-block-to-ir (example-block)
  "Convert an EXAMPLE-BLOCK to semantic example-block IR."
  (let* ((value (or (org-element-property :value example-block) ""))
         (lines (gdocs-convert-code--body-lines value)))
    (list :type 'example-block :lines lines :id (gdocs-convert--next-id))))

(defun gdocs-convert--lines-to-monospace-ir (lines)
  "Convert LINES to individual code paragraphs, including literal environments."
  (mapcar (lambda (line)
            (list :type 'paragraph :style 'normal
                  :contents (list (gdocs-convert--make-run line (gdocs-convert--code-props)))
                  :id (gdocs-convert--next-id))) lines))

(defun gdocs-convert--src-block-data (element)
  "Return source-block marker data from ELEMENT, or nil."
  (gdocs-convert--extract-marker-field (plist-get element :gdocs-marker) 'src-block))

(defun gdocs-convert--example-block-data (element)
  "Return example-block marker data from ELEMENT, or nil."
  (gdocs-convert--extract-marker-field (plist-get element :gdocs-marker) 'example-block))

(defun gdocs-convert--src-block-paragraph-p (element)
  "Return non-nil if ELEMENT is a marked source-block paragraph."
  (and (eq (plist-get element :type) 'paragraph) (gdocs-convert--src-block-data element)))

(defun gdocs-convert--example-block-paragraph-p (element)
  "Return non-nil if ELEMENT is a marked example-block paragraph."
  (and (eq (plist-get element :type) 'paragraph) (gdocs-convert--example-block-data element)))

(defun gdocs-convert-code-blocks--source-marker-id (element)
  "Return the native source block identity carried by ELEMENT."
  (plist-get (seq-find (lambda (marker) (eq (plist-get marker :type) 'src-block))
                       (plist-get element :gdocs-marker)) :id))

(defun gdocs-convert-code-blocks--attach-marked-lines (ir type predicate key-function)
  "Group IR lines of TYPE recognized by PREDICATE and KEY-FUNCTION.
Source grouping requires matching language and native block identity."
  (let (result (remaining ir))
    (while remaining
      (let ((element (car remaining)))
        (if (funcall predicate element)
            (let ((key (funcall key-function element)) lines
                  (source-id (gdocs-convert-code-blocks--source-marker-id element))
                  (start (plist-get element :doc-start)) end)
              (while (and remaining (funcall predicate (car remaining))
                          (equal key (funcall key-function (car remaining)))
                          (or (not (eq type 'source-block))
                              (equal source-id (gdocs-convert-code-blocks--source-marker-id (car remaining)))))
                (push (gdocs-convert--plain-text (car remaining)) lines)
                (setq end (plist-get (car remaining) :doc-end) remaining (cdr remaining)))
              (push (append (if (eq type 'source-block)
                                (list :type type :language key :lines (nreverse lines) :id (gdocs-convert--next-id))
                              (list :type type :lines (nreverse lines) :id (gdocs-convert--next-id)))
                            (when start (list :doc-start start)) (when end (list :doc-end end))) result))
          (push element result)
          (setq remaining (cdr remaining)))))
    (nreverse result)))

(defun gdocs-convert--attach-source-block-markers (ir)
  "Group consecutive marked source-block paragraphs in IR."
  (gdocs-convert-code-blocks--attach-marked-lines
   ir 'source-block #'gdocs-convert--src-block-paragraph-p #'gdocs-convert--src-block-data))

(defun gdocs-convert--attach-example-block-markers (ir)
  "Group consecutive marked example-block paragraphs in IR."
  (gdocs-convert-code-blocks--attach-marked-lines
   ir 'example-block #'gdocs-convert--example-block-paragraph-p #'gdocs-convert--example-block-data))

(defun gdocs-convert--all-code-paragraph-p (element)
  "Return non-nil for unmarked, unaligned normal all-code paragraph ELEMENT."
  (and (eq (plist-get element :type) 'paragraph) (eq (plist-get element :style) 'normal)
       (not (plist-get element :list)) (not (plist-get element :gdocs-marker))
       (not (plist-get element :alignment))
       (let ((runs (plist-get element :contents)))
         (and runs (seq-every-p (lambda (run) (plist-get run :code)) runs)))))

(defun gdocs-convert-code-block-org-group (elements start)
  "Describe a consecutive legacy code group in ELEMENTS at START, or nil.
The descriptor supplies :next-index, :org-text, :element and :blank-before."
  (when (gdocs-convert--all-code-paragraph-p (aref elements start))
    (let ((index start) lines)
      (while (and (< index (length elements))
                  (gdocs-convert--all-code-paragraph-p (aref elements index)))
        (push (gdocs-convert--plain-text (aref elements index)) lines)
        (setq index (1+ index)))
      (list :next-index index :element (list :type 'example-block) :blank-before t
            :org-text (format "#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE"
                              (mapconcat #'identity (nreverse lines) "\n"))))))

(defun gdocs-convert--format-src-block-from-lines (language lines)
  "Format a source block with LANGUAGE from code LINES."
  (concat "#+BEGIN_SRC" (if (and language (not (string-empty-p language))) (concat " " language) "")
          "\n" (mapconcat #'identity lines "\n") "\n#+END_SRC"))

(defun gdocs-convert--format-src-block (data element)
  "Format a src block from marker DATA and paragraph ELEMENT."
  (let* ((legacy-data-p (listp data))
         (language (if legacy-data-p (plist-get data :language) data))
         (value (if legacy-data-p (plist-get data :value)
                  (concat (gdocs-convert--plain-text element) "\n"))))
    (gdocs-convert--format-src-block-from-lines
     language (gdocs-convert-code--body-lines value))))

(defun gdocs-convert--ir-source-block-to-org (element)
  "Convert source-block IR ELEMENT to Org source block text."
  (gdocs-convert--format-src-block-from-lines (plist-get element :language) (plist-get element :lines)))

(defun gdocs-convert--ir-example-block-to-org (element)
  "Convert example-block IR ELEMENT to Org example block text."
  (format "#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE" (mapconcat #'identity (or (plist-get element :lines) '("")) "\n")))

(defun gdocs-convert--source-block-line-element (element line line-index line-count)
  "Return paragraph IR for source-block ELEMENT LINE at LINE-INDEX."
  (gdocs-convert--typographic-block-paragraph
   element (list (gdocs-convert--make-run line (gdocs-convert--code-props)))
   'gdocs-source-block (list :type 'src-block :data (or (plist-get element :language) ""))
   line-index line-count))

(defun gdocs-convert--example-block-line-element (element line line-index line-count)
  "Return paragraph IR for example-block ELEMENT LINE at LINE-INDEX."
  (gdocs-convert--typographic-block-paragraph
   element (list (gdocs-convert--make-run line (gdocs-convert--code-props)))
   'gdocs-source-block (list :type 'example-block :data (plist-get element :id)) line-index line-count))

(defun gdocs-convert-code-blocks--requests (element index line-function)
  "Plan ELEMENT at INDEX with feature-owned LINE-FUNCTION."
  (let* ((lines (or (plist-get element :lines) '(""))) (count (length lines))
         (line-index 0) requests (current index))
    (dolist (line lines)
      (let* ((paragraph (funcall line-function element line line-index count))
             (result (gdocs-convert--paragraph-to-requests paragraph current)))
        (setq requests (nconc requests (plist-get result :requests)) current (plist-get result :index))
        (cl-incf line-index)))
    (list :requests requests :index current)))

(defun gdocs-convert--source-block-to-requests (element index)
  "Create paragraph requests for source-block ELEMENT at INDEX."
  (gdocs-convert-code-blocks--requests element index #'gdocs-convert--source-block-line-element))

(defun gdocs-convert--example-block-to-requests (element index)
  "Create paragraph requests for example-block ELEMENT at INDEX."
  (gdocs-convert-code-blocks--requests element index #'gdocs-convert--example-block-line-element))

(declare-function gdocs-convert-inline-plain-run-to-org "gdocs-convert-inline-codec" (run))
(declare-function gdocs-convert--runs-same-format-p "gdocs-convert-inline-codec" (a b))

(defun gdocs-convert--inline-src-block-to-runs (object inherited-props)
  "Convert inline source OBJECT without discarding language or parameters."
  (when (org-element-lineage object '(table footnote-definition footnote-reference))
    (user-error "Inline source in tables or footnotes cannot preserve language metadata yet"))
  (let ((run (gdocs-convert--make-run (org-element-property :value object)
                                      (gdocs-convert--code-props inherited-props))))
    (list (plist-put run :inline-source
                     (list :language (org-element-property :language object)
                           :parameters (org-element-property :parameters object))))))

(defun gdocs-convert-code-run-policy (run)
  "Return normalization policy protecting inline source RUN's syntax."
  (if-let* ((source (plist-get run :inline-source)))
      (list :atomic t :identity source :preserve-whitespace t :preserve-terminal t :formatted t)
    (when (plist-get run :code) '(:preserve-whitespace t))))

(defun gdocs-convert-code-render-run (run)
  "Render inline source RUN with its authored language and header arguments."
  (when-let* ((source (plist-get run :inline-source)))
    (let ((plain (copy-sequence run)))
      (plist-put plain :code nil)
      (plist-put plain :text
                 (format "src_%s%s{%s}" (plist-get source :language)
                         (if-let* ((parameters (plist-get source :parameters))) (concat "[" parameters "]") "")
                         (plist-get run :text)))
      (gdocs-convert-inline-plain-run-to-org plain))))

(defun gdocs-convert-code-run-marker-request (run start end &optional segment-id)
  "Return inline source RUN metadata at START..END, failing before lossy upload."
  (when-let* ((source (plist-get run :inline-source)))
    (when (or segment-id (= start end) (string-match-p "[\n\r]" (plist-get run :text)))
      (user-error "Inline source requires nonempty single-paragraph body text"))
    (let* ((data (url-hexify-string (json-serialize
                                     (list (cons 'language (plist-get source :language))
                                           (cons 'parameters (or (plist-get source :parameters) ""))))))
           (name (format "gdocs-org-marker:inline-src-block:run-%d:%s" start data)))
      (when (> (length name) 256)
        (user-error "Inline source language/header metadata exceeds the native named-range limit"))
      `((createNamedRange . ((name . ,name) (range . ((startIndex . ,start) (endIndex . ,end)))))))))

(defun gdocs-convert-code-validate-native-table (element markers)
  "Reject inline source metadata in native table ELEMENT before lossy decoding."
  (let ((start (alist-get 'startIndex element)) (end (alist-get 'endIndex element)))
    (when (seq-some (lambda (marker)
                      (and (eq (plist-get marker :type) 'inline-src-block)
                           start end (< (plist-get marker :start) end) (> (plist-get marker :end) start))) markers)
      (user-error "Inline source in native tables cannot preserve language metadata yet"))))

(defun gdocs-convert-code-validate-native-marker (type ranges entry)
  "Validate inline-source TYPE RANGES in native named-range ENTRY."
  (when (eq type 'inline-src-block)
    (unless (and (or (listp ranges) (vectorp ranges)) (= (length ranges) 1))
      (user-error "Inline source metadata requires one contiguous native range"))
    (let ((range (elt ranges 0)))
      (unless (and (listp range) (listp entry)
                   (stringp (alist-get 'namedRangeId entry))
                   (integerp (alist-get 'startIndex range))
                   (integerp (alist-get 'endIndex range))
                   (< (alist-get 'startIndex range) (alist-get 'endIndex range))
                   (not (alist-get 'segmentId range)))
        (user-error "Malformed inline source range or unsupported document segment")))))

(defun gdocs-convert-code--marker-source (marker)
  "Decode and validate inline source MARKER metadata."
  (let* ((data (condition-case nil
                   (json-parse-string (decode-coding-string (url-unhex-string (or (plist-get marker :data) "")) 'utf-8)
                                      :object-type 'alist)
                 (error (user-error "Malformed inline source language metadata"))))
         (object-p (and (listp data) (cl-every #'consp data)))
         (language (when object-p (alist-get 'language data)))
         (parameters (when object-p (alist-get 'parameters data))))
    (unless (and (stringp language) (string-match-p "\\`[[:alnum:]_+.-]+\\'" language)
                 (stringp parameters) (not (string-match-p "[\n\r]" parameters)))
      (user-error "Invalid inline source language/header metadata"))
    (list :language language :parameters (unless (string-empty-p parameters) parameters))))

(defun gdocs-convert-code-paragraph-markers (markers)
  "Exclude inline-source MARKERS from paragraph-level annotations."
  (seq-remove (lambda (marker) (eq (plist-get marker :type) 'inline-src-block)) markers))

(defun gdocs-convert-code-native-parts (element markers parts)
  "Restore inline source ranges in native text ELEMENT's decoded PARTS.
MARKERS are explicit document context; other feature parts remain untouched."
  (let ((sources (seq-filter (lambda (marker) (eq (plist-get marker :type) 'inline-src-block)) markers)))
    (if (or (not (alist-get 'textRun element)) (null sources)) parts
      (let ((position (or (alist-get 'startIndex element) 0)) result)
        (dolist (part parts)
          (let* ((run (cdr part)) (text (plist-get run :text))
                 (end (+ position (gdocs-convert--string-to-utf16-length text)))
                 (overlapping (seq-filter (lambda (marker) (and (< (plist-get marker :start) end)
                                                                (> (plist-get marker :end) position))) sources))
                 (cuts (sort (delete-dups
                              (append (list position end)
                                      (cl-mapcan (lambda (marker) (list (max position (plist-get marker :start))
                                                                        (min end (plist-get marker :end)))) overlapping))) #'<)))
            (while (cdr cuts)
              (let* ((a (car cuts)) (b (cadr cuts))
                     (covering (seq-filter (lambda (marker) (and (<= (plist-get marker :start) a)
                                                                 (>= (plist-get marker :end) b))) overlapping))
                     (marker (car covering)) (copy (copy-sequence run)))
                (when (or (> (length covering) 1) (and marker (car part)))
                  (user-error "Overlapping inline source semantic markers require review"))
                (plist-put copy :text (substring text (gdocs-convert--utf16-string-index text (- a position))
                                                 (gdocs-convert--utf16-string-index text (- b position))))
                (when marker
                  (when (string-match-p "[\n\r]" (plist-get copy :text))
                    (user-error "Inline source marker crosses a paragraph boundary"))
                  (plist-put copy :code t)
                  (plist-put copy :style-names '(gdocs-code))
                  (plist-put copy :inline-source (gdocs-convert-code--marker-source marker))
                  (plist-put copy :inline-source-id (plist-get marker :id))
                  (plist-put copy :inline-source-range-ids (when (plist-get marker :range-id) (list (plist-get marker :range-id)))))
                (push (cons (car part) copy) result))
              (setq cuts (cdr cuts)))
            (setq position end)))
        (nreverse result)))))

(defun gdocs-convert-code-finalize-runs (runs)
  "Coalesce native fragments of one inline source object in RUNS without data loss."
  (let (result)
    (dolist (run runs)
      (let ((previous (car result)) (id (plist-get run :inline-source-id)))
        (if (and id previous (equal id (plist-get previous :inline-source-id)))
            (let ((a (copy-sequence previous)) (b (copy-sequence run)))
              (plist-put a :inline-source nil) (plist-put b :inline-source nil)
              (unless (and (equal (plist-get previous :inline-source) (plist-get run :inline-source))
                           (gdocs-convert--runs-same-format-p a b))
                (user-error "Inline source has mixed native formatting that Org literal code cannot preserve"))
              (plist-put previous :text (concat (plist-get previous :text) (plist-get run :text)))
              (plist-put previous :inline-source-range-ids
                         (delete-dups (append (plist-get previous :inline-source-range-ids)
                                              (plist-get run :inline-source-range-ids)))))
          (push (copy-sequence run) result))))
    (nreverse result)))

(provide 'gdocs-convert-code-blocks)
;;; gdocs-convert-code-blocks.el ends here
