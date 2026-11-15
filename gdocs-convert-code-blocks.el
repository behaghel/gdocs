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

(defun gdocs-convert--src-block-to-ir (src-block)
  "Convert a SRC-BLOCK to a semantic source-block IR element."
  (let* ((language (or (org-element-property :language src-block) ""))
         (value (save-match-data (string-trim-right (or (org-element-property :value src-block) ""))))
         (lines (split-string value "\n")))
    (list :type 'source-block :language language :lines lines :id (gdocs-convert--next-id))))

(defun gdocs-convert--example-block-to-ir (example-block)
  "Convert an EXAMPLE-BLOCK to semantic example-block IR."
  (let* ((value (or (org-element-property :value example-block) ""))
         (lines (split-string (save-match-data (string-trim-right value)) "\n")))
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

(defun gdocs-convert-code-blocks--attach-marked-lines (ir type predicate key-function)
  "Group IR lines of TYPE recognized by PREDICATE and KEY-FUNCTION.
Source blocks retain language-based grouping; example blocks retain block IDs."
  (let (result (remaining ir))
    (while remaining
      (let ((element (car remaining)))
        (if (funcall predicate element)
            (let ((key (funcall key-function element)) lines
                  (start (plist-get element :doc-start)) end)
              (while (and remaining (funcall predicate (car remaining))
                          (equal key (funcall key-function (car remaining))))
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
     language (split-string (save-match-data (string-trim-right value)) "\n"))))

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

(provide 'gdocs-convert-code-blocks)
;;; gdocs-convert-code-blocks.el ends here
