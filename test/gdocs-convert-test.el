;;; gdocs-convert-test.el --- Tests for gdocs-convert.el -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the bidirectional org <-> IR <-> Google Docs conversion.

;;; Code:

(require 'ert)
(require 'gdocs-convert)

;; ---------------------------------------------------------------------------
;;; Helpers

(defun gdocs-convert-test--first-content-run (ir)
  "Return the first text run from the first element of IR."
  (car (plist-get (car ir) :contents)))

(defun gdocs-convert-test--find-element (ir type &optional style)
  "Find the first IR element with TYPE and optional STYLE in IR."
  (--first (and (eq (plist-get it :type) type)
                (or (null style) (eq (plist-get it :style) style)))
           ir))

;; ---------------------------------------------------------------------------
;;; Basic paragraph

(ert-deftest gdocs-convert-test-basic-paragraph ()
  "A simple paragraph produces an IR element with :style normal."
  (let ((ir (gdocs-convert-org-string-to-ir "Hello world")))
    (should (= (length ir) 1))
    (should (eq (plist-get (car ir) :type) 'paragraph))
    (should (eq (plist-get (car ir) :style) 'normal))
    (should (string= (plist-get (gdocs-convert-test--first-content-run ir)
                                :text)
                      "Hello world"))))

;; ---------------------------------------------------------------------------
;;; Heading levels

(ert-deftest gdocs-convert-test-heading-levels ()
  "Heading levels 1-6 map to heading-1 through heading-6."
  (dolist (level '(1 2 3 4 5 6))
    (let* ((stars (make-string level ?*))
           (org-str (format "%s Heading %d" stars level))
           (ir (gdocs-convert-org-string-to-ir org-str))
           (elem (car ir)))
      (should (eq (plist-get elem :style)
                  (intern (format "heading-%d" level)))))))

(ert-deftest gdocs-convert-test-heading-level-7-clamped ()
  "Heading level 7+ is clamped to heading-6."
  (let* ((ir (gdocs-convert-org-string-to-ir "******* Deep heading"))
         (elem (car ir)))
    (should (eq (plist-get elem :style) 'heading-6))))

;; ---------------------------------------------------------------------------
;;; Text formatting

(ert-deftest gdocs-convert-test-bold ()
  "Bold markup produces a run with :bold t."
  (let* ((ir (gdocs-convert-org-string-to-ir "This is *bold* text"))
         (runs (plist-get (car ir) :contents))
         (bold-run (nth 1 runs)))
    (should (plist-get bold-run :bold))
    (should (string= (plist-get bold-run :text) "bold"))))

(ert-deftest gdocs-convert-test-italic ()
  "Italic markup produces a run with :italic t."
  (let* ((ir (gdocs-convert-org-string-to-ir "This is /italic/ text"))
         (runs (plist-get (car ir) :contents))
         (italic-run (nth 1 runs)))
    (should (plist-get italic-run :italic))
    (should (string= (plist-get italic-run :text) "italic"))))

(ert-deftest gdocs-convert-test-underline ()
  "Underline markup produces a run with :underline t."
  (let* ((ir (gdocs-convert-org-string-to-ir "This is _underlined_ text"))
         (runs (plist-get (car ir) :contents))
         (underline-run (nth 1 runs)))
    (should (plist-get underline-run :underline))
    (should (string= (plist-get underline-run :text) "underlined"))))

(ert-deftest gdocs-convert-test-strikethrough ()
  "Strikethrough markup produces a run with :strikethrough t."
  (let* ((ir (gdocs-convert-org-string-to-ir "This is +struck+ text"))
         (runs (plist-get (car ir) :contents))
         (strike-run (nth 1 runs)))
    (should (plist-get strike-run :strikethrough))
    (should (string= (plist-get strike-run :text) "struck"))))

(ert-deftest gdocs-convert-test-code ()
  "Code markup produces a run with :code t."
  (let* ((ir (gdocs-convert-org-string-to-ir "This is ~code~ text"))
         (runs (plist-get (car ir) :contents))
         (code-run (nth 1 runs)))
    (should (plist-get code-run :code))
    (should (string= (plist-get code-run :text) "code"))))

(ert-deftest gdocs-convert-test-verbatim ()
  "Verbatim markup produces a run with :code t."
  (let* ((ir (gdocs-convert-org-string-to-ir "This is =verbatim= text"))
         (runs (plist-get (car ir) :contents))
         (code-run (nth 1 runs)))
    (should (plist-get code-run :code))
    (should (string= (plist-get code-run :text) "verbatim"))))

(ert-deftest gdocs-convert-test-link ()
  "Link markup produces a run with :link set."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "Visit [[https://example.com][Example]]"))
         (runs (plist-get (car ir) :contents))
         (link-run (nth 1 runs)))
    (should (string= (plist-get link-run :link) "https://example.com"))
    (should (string= (plist-get link-run :text) "Example"))))

;; ---------------------------------------------------------------------------
;;; Nested formatting

(ert-deftest gdocs-convert-test-bold-italic ()
  "Nested bold+italic produces a run with both :bold t and :italic t."
  (let* ((ir (gdocs-convert-org-string-to-ir "This is */bold-italic/* text"))
         (runs (plist-get (car ir) :contents))
         (bi-run (nth 1 runs)))
    (should (plist-get bi-run :bold))
    (should (plist-get bi-run :italic))
    (should (string= (plist-get bi-run :text) "bold-italic"))))

;; ---------------------------------------------------------------------------
;;; Unordered lists

(ert-deftest gdocs-convert-test-unordered-list ()
  "Unordered list items produce IR elements with :list info."
  (let* ((ir (gdocs-convert-org-string-to-ir "- First\n- Second\n- Third"))
         (first-elem (car ir)))
    (should (>= (length ir) 3))
    (should (eq (plist-get (plist-get first-elem :list) :type) 'bullet))
    (should (= (plist-get (plist-get first-elem :list) :level) 0))))

(ert-deftest gdocs-convert-test-nested-list ()
  "Nested list items have increasing :level."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "- Parent\n  - Child\n    - Grandchild")))
    (should (>= (length ir) 3))
    (should (= (plist-get (plist-get (nth 0 ir) :list) :level) 0))
    (should (= (plist-get (plist-get (nth 1 ir) :list) :level) 1))
    (should (= (plist-get (plist-get (nth 2 ir) :list) :level) 2))))

;; ---------------------------------------------------------------------------
;;; Ordered lists

(ert-deftest gdocs-convert-test-ordered-list ()
  "Ordered list items produce IR elements with :type number."
  (let* ((ir (gdocs-convert-org-string-to-ir "1. First\n2. Second"))
         (first-elem (car ir)))
    (should (>= (length ir) 2))
    (should (eq (plist-get (plist-get first-elem :list) :type) 'number))))

(ert-deftest gdocs-convert-test-nested-ordered-list ()
  "Nested ordered list items have increasing :level with type number."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "1. Parent\n   1. Child\n      1. Grandchild")))
    (should (>= (length ir) 3))
    (should (eq (plist-get (plist-get (nth 0 ir) :list) :type) 'number))
    (should (= (plist-get (plist-get (nth 0 ir) :list) :level) 0))
    (should (= (plist-get (plist-get (nth 1 ir) :list) :level) 1))
    (should (= (plist-get (plist-get (nth 2 ir) :list) :level) 2))))

;; ---------------------------------------------------------------------------
;;; Checkbox lists

(ert-deftest gdocs-convert-test-checkbox-unchecked ()
  "Unchecked checkbox items produce :type check, :checked nil."
  (let* ((ir (gdocs-convert-org-string-to-ir "- [ ] Todo item"))
         (elem (car ir))
         (list-info (plist-get elem :list)))
    (should (eq (plist-get list-info :type) 'check))
    (should-not (plist-get list-info :checked))))

(ert-deftest gdocs-convert-test-checkbox-checked ()
  "Checked checkbox items produce :type check, :checked t."
  (let* ((ir (gdocs-convert-org-string-to-ir "- [X] Done item"))
         (elem (car ir))
         (list-info (plist-get elem :list)))
    (should (eq (plist-get list-info :type) 'check))
    (should (plist-get list-info :checked))))

;; ---------------------------------------------------------------------------
;;; Tables

(ert-deftest gdocs-convert-test-table ()
  "A 2x2 org table produces an IR table element with :rows."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "| A | B |\n|---+---|\n| C | D |"))
         (table-elem (gdocs-convert-test--find-element ir 'table)))
    (should table-elem)
    (let ((rows (plist-get table-elem :rows)))
      (should (= (length rows) 2))
      (should (= (length (car rows)) 2))
      (should (string= (plist-get (car (car (car rows))) :text) "A")))))

(ert-deftest gdocs-convert-test-table-column-max-lengths ()
  "Column max lengths are computed from cell text."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "| A | Long text |\n|---+---|\n| CD | E |"))
         (table-elem (gdocs-convert-test--find-element ir 'table))
         (rows (plist-get table-elem :rows))
         (lengths (gdocs-convert--table-column-max-lengths rows)))
    (should (= (length lengths) 2))
    ;; Col 0: max("A","CD") = 2, Col 1: max("Long text","E") = 9
    (should (= (nth 0 lengths) 2))
    (should (= (nth 1 lengths) 9))))

(ert-deftest gdocs-convert-test-table-column-widths-proportional ()
  "Column widths are proportional to max content lengths."
  (let ((widths (gdocs-convert--table-column-widths '(10 30))))
    (should (= (length widths) 2))
    ;; Second column should be roughly 3x the first (both above minimum).
    (should (> (nth 1 widths) (* 2.0 (nth 0 widths))))))

(ert-deftest gdocs-convert-test-table-column-widths-minimum ()
  "No column width is below the minimum."
  (let ((widths (gdocs-convert--table-column-widths '(1 1000))))
    (should (>= (nth 0 widths) gdocs-convert--table-min-column-width-pt))))

(ert-deftest gdocs-convert-test-table-column-width-requests ()
  "Table requests include updateTableColumnProperties."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "| Tier | Notes with longer text |\n|---+---|\n| 1 | Some description |"))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (col-reqs (cl-remove-if-not
                    (lambda (r) (alist-get 'updateTableColumnProperties r))
                    requests)))
    ;; Should have 2 column width requests (one per column).
    (should (= (length col-reqs) 2))
    ;; Each should have tableStartLocation, columnIndices, and tableColumnProperties.
    (let ((req (alist-get 'updateTableColumnProperties (car col-reqs))))
      (should (alist-get 'tableStartLocation req))
      (should (alist-get 'columnIndices req))
      (should (alist-get 'tableColumnProperties req)))))

;; ---------------------------------------------------------------------------
;;; Horizontal rules

(ert-deftest gdocs-convert-test-horizontal-rule ()
  "A horizontal rule produces an IR element with :type horizontal-rule."
  (let* ((ir (gdocs-convert-org-string-to-ir "Before\n-----\nAfter"))
         (hr (gdocs-convert-test--find-element ir 'horizontal-rule)))
    (should hr)
    (should (eq (plist-get hr :type) 'horizontal-rule))))

;; ---------------------------------------------------------------------------
;;; Block quotes

(ert-deftest gdocs-convert-test-quote-block ()
  "A quote block produces semantic quote-block IR."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "#+BEGIN_QUOTE\nTo be or not to be.\n#+END_QUOTE"))
         (quote-elem (car ir)))
    (should (eq (plist-get quote-elem :type) 'quote-block))
    (should (equal (mapcar #'gdocs-convert--runs-to-plain-text
                           (plist-get quote-elem :paragraphs))
                   '("To be or not to be.")))))

;; ---------------------------------------------------------------------------
;;; Round-trip fidelity

(ert-deftest gdocs-convert-test-round-trip-simple ()
  "A simple document survives org -> IR -> org round-trip."
  (let* ((original "#+TITLE: My document\n\n* Introduction\n\nHello world.\n")
         (ir (gdocs-convert-org-string-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    (should (string= (s-trim result) (s-trim original)))))

(ert-deftest gdocs-convert-test-round-trip-formatting ()
  "Formatted text survives round-trip."
  (let* ((original "This has *bold*, /italic/, and ~code~ text.\n")
         (ir (gdocs-convert-org-string-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    (should (string= (s-trim result) (s-trim original)))))

(ert-deftest gdocs-convert-test-round-trip-lists ()
  "List items survive round-trip."
  (let* ((original "- First\n- Second\n- Third\n")
         (ir (gdocs-convert-org-string-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    (should (string= (s-trim result) (s-trim original)))))

(ert-deftest gdocs-convert-test-round-trip-nested-lists ()
  "Nested list items survive round-trip."
  (let* ((original "- Parent\n  - Child\n    - Grandchild\n")
         (ir (gdocs-convert-org-string-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    (should (string= (s-trim result) (s-trim original)))))

(ert-deftest gdocs-convert-test-round-trip-nested-ordered-lists ()
  "Nested ordered list items survive round-trip with correct nesting."
  (let* ((original "1. Parent\n   1. Child\n      1. Grandchild\n")
         (ir (gdocs-convert-org-string-to-ir original))
         (result (gdocs-convert-ir-to-org ir))
         ;; Re-parse the output to verify nesting survives
         (re-ir (gdocs-convert-org-string-to-ir result)))
    (should (= (length re-ir) 3))
    (should (= (plist-get (plist-get (nth 0 re-ir) :list) :level) 0))
    (should (= (plist-get (plist-get (nth 1 re-ir) :list) :level) 1))
    (should (= (plist-get (plist-get (nth 2 re-ir) :list) :level) 2))))

(ert-deftest gdocs-convert-test-round-trip-table ()
  "A table survives round-trip."
  (let* ((original "| A | B |\n|---+---|\n| C | D |\n")
         (ir (gdocs-convert-org-string-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    (should (string= (s-trim result) (s-trim original)))))

;; ---------------------------------------------------------------------------
;;; List nesting requests

(ert-deftest gdocs-convert-test-nested-list-indent-requests ()
  "Nested list items generate updateParagraphStyle indent requests."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "- Parent\n  - Child\n    - Grandchild"))
         (requests (gdocs-convert-ir-to-docs-requests ir)))
    ;; Level 0: only createParagraphBullets, no indent request
    (should (cl-some (lambda (r) (alist-get 'createParagraphBullets r))
                     requests))
    ;; Level 1: indent request with indentStart=72, indentFirstLine=54
    (let ((indent-reqs
           (cl-remove-if-not
            (lambda (r)
              (let ((ups (alist-get 'updateParagraphStyle r)))
                (when ups
                  (let ((style (alist-get 'paragraphStyle ups)))
                    (alist-get 'indentStart style)))))
            requests)))
      (should (>= (length indent-reqs) 2))  ;; level 1 and level 2
      ;; Check level 1 indentation (72pt start, 54pt first)
      (let* ((req1 (car indent-reqs))
             (style (alist-get 'paragraphStyle
                               (alist-get 'updateParagraphStyle req1)))
             (start-mag (alist-get 'magnitude (alist-get 'indentStart style)))
             (first-mag (alist-get 'magnitude
                                   (alist-get 'indentFirstLine style))))
        (should (= start-mag 72))
        (should (= first-mag 54)))
      ;; Check level 2 indentation (108pt start, 90pt first)
      (let* ((req2 (cadr indent-reqs))
             (style (alist-get 'paragraphStyle
                               (alist-get 'updateParagraphStyle req2)))
             (start-mag (alist-get 'magnitude (alist-get 'indentStart style)))
             (first-mag (alist-get 'magnitude
                                   (alist-get 'indentFirstLine style))))
        (should (= start-mag 108))
        (should (= first-mag 90))))))

;; ---------------------------------------------------------------------------
;;; Google Docs JSON -> IR

(ert-deftest gdocs-convert-test-docs-json-paragraph ()
  "A Google Docs paragraph converts to a normal IR paragraph."
  (let* ((json `((title . "Test Doc")
                 (body . ((content
                           . [((paragraph
                                . ((elements
                                    . [((textRun
                                         . ((content . "Hello\n")
                                            (textStyle . ()))))])
                                   (paragraphStyle
                                    . ((namedStyleType . "NORMAL_TEXT"))))))])))))
         (ir (gdocs-convert-docs-json-to-ir json)))
    (should (>= (length ir) 2))
    (let ((para (nth 1 ir)))
      (should (eq (plist-get para :type) 'paragraph))
      (should (eq (plist-get para :style) 'normal))
      (should (string= (plist-get (car (plist-get para :contents)) :text)
                        "Hello")))))

(ert-deftest gdocs-convert-test-docs-json-heading ()
  "A Google Docs HEADING_1 converts to :style heading-1."
  (let* ((json `((body . ((content
                           . [((paragraph
                                . ((elements
                                    . [((textRun
                                         . ((content . "Title\n")
                                            (textStyle . ()))))])
                                   (paragraphStyle
                                    . ((namedStyleType . "HEADING_1"))))))])))))
         (ir (gdocs-convert-docs-json-to-ir json))
         (heading (car ir)))
    (should (eq (plist-get heading :style) 'heading-1))))

(ert-deftest gdocs-convert-test-docs-json-inline-image-preserves-uri ()
  "Google Docs inline images convert to image IR with source/content URIs."
  (let* ((json `((body . ((content
                           . [((paragraph
                                . ((elements
                                    . [((inlineObjectElement
                                         . ((inlineObjectId . "img-1"))))])
                                   (paragraphStyle
                                    . ((namedStyleType . "NORMAL_TEXT"))))))])))
                 (inlineObjects
                  . ((img-1
                      . ((inlineObjectProperties
                          . ((embeddedObject
                              . ((title . "Logo")
                                 (description . "Product logo")
                                 (imageProperties
                                  . ((contentUri
                                      . "https://lh3.googleusercontent.com/image")
                                     (sourceUri
                                      . "https://example.invalid/logo.png")))))))))))))
         (ir (gdocs-convert-docs-json-to-ir json))
         (image (car ir)))
    (should (eq (plist-get image :type) 'image))
    (should (equal (plist-get image :object-id) "img-1"))
    (should (equal (plist-get image :source-uri)
                   "https://example.invalid/logo.png"))
    (should (equal (plist-get image :content-uri)
                   "https://lh3.googleusercontent.com/image"))
    (should (equal (plist-get image :title) "Logo"))
    (should (equal (plist-get image :description) "Product logo"))))

(ert-deftest gdocs-convert-test-docs-json-marked-image-caption ()
  "Marked caption paragraphs attach to preceding image IR on pull."
  (let* ((body-elements
          (vector `((startIndex . 1)
                    (endIndex . 2)
                    (paragraph
                     . ((elements
                         . ,(vector '((inlineObjectElement
                                      . ((inlineObjectId . "img-1"))))))
                        (paragraphStyle
                         . ((namedStyleType . "NORMAL_TEXT"))))))
                  `((startIndex . 2)
                    (endIndex . 16)
                    (paragraph
                     . ((elements
                         . ,(vector '((textRun
                                      . ((content . "Hubert banner\n")
                                         (textStyle . ()))))))
                        (paragraphStyle
                         . ((namedStyleType . "NORMAL_TEXT"))))))))
         (json `((body . ((content . ,body-elements)))
                 (inlineObjects
                  . ((img-1
                      . ((inlineObjectProperties
                          . ((embeddedObject
                              . ((imageProperties
                                  . ((sourceUri
                                      . "https://example.invalid/logo.png")))))))))))
                 (namedRanges
                  . ((gdocs-org-marker:image-caption:img-1:org-image-caption
                      . ((namedRanges
                          . ,(vector `((ranges
                                        . ,(vector '((startIndex . 2)
                                                     (endIndex . 15)))))))))))))
         (ir (gdocs-convert-docs-json-to-ir json))
         (image (car ir)))
    (should (= (length ir) 1))
    (should (eq (plist-get image :type) 'image))
    (should (equal (plist-get image :caption) "Hubert banner"))
    (should (= (plist-get image :doc-start) 1))
    (should (= (plist-get image :doc-end) 16))
    (should (equal (gdocs-convert-ir-to-org ir)
                   "#+CAPTION: Hubert banner\n[[https://example.invalid/logo.png]]\n"))))

(ert-deftest gdocs-convert-test-docs-json-marked-src-block ()
  "Marked source-block paragraphs pull back to Org source blocks."
  (let* ((body-elements
          (vector `((startIndex . 1)
                    (endIndex . 10)
                    (paragraph
                     . ((elements
                         . ,(vector '((textRun
                                      . ((content . "print(1)\n")
                                         (textStyle
                                          . ((weightedFontFamily
                                              . ((fontFamily . "Courier New"))))))))))
                        (paragraphStyle
                         . ((namedStyleType . "NORMAL_TEXT"))))))))
         (json `((body . ((content . ,body-elements)))
                 (namedRanges
                  . ((gdocs-org-marker:src-block:elem-001:python
                      . ((namedRanges
                          . ,(vector `((ranges
                                        . ,(vector '((startIndex . 1)
                                                     (endIndex . 9)))))))))))))
         (ir (gdocs-convert-docs-json-to-ir json))
         (block (car ir))
         (org (gdocs-convert-ir-to-org ir)))
    (should (= (length ir) 1))
    (should (eq (plist-get block :type) 'source-block))
    (should (equal (plist-get block :language) "python"))
    (should (equal (plist-get block :lines) '("print(1)")))
    (should (= (plist-get block :doc-start) 1))
    (should (= (plist-get block :doc-end) 10))
    (should (string= (s-trim org)
                     "#+BEGIN_SRC python\nprint(1)\n#+END_SRC"))))

(ert-deftest gdocs-convert-test-docs-json-marked-quote-block-consumes-separator ()
  "Marked quote blocks consume generated trailing separator paragraphs."
  (let* ((body-elements
          (vector `((startIndex . 1)
                    (endIndex . 8)
                    (paragraph
                     . ((elements . ,(vector '((textRun . ((content . "Quote.\n")
                                                            (textStyle . ()))))))
                        (paragraphStyle . ((namedStyleType . "NORMAL_TEXT"))))))
                  `((startIndex . 8)
                    (endIndex . 9)
                    (paragraph
                     . ((elements . ,(vector '((textRun . ((content . "\n")
                                                            (textStyle . ()))))))
                        (paragraphStyle . ((namedStyleType . "NORMAL_TEXT"))))))))
         (json `((body . ((content . ,body-elements)))
                 (namedRanges
                  . ,(list
                      (cons (intern "gdocs-org-marker:quote-block:elem-001")
                            `((namedRanges . ,(vector `((ranges . ,(vector '((startIndex . 1) (endIndex . 7)))))))))))))
         (ir (gdocs-convert-docs-json-to-ir json)))
    (should (= (length ir) 1))
    (should (eq (plist-get (car ir) :type) 'quote-block))
    (should (= (plist-get (car ir) :doc-end) 9))))

(ert-deftest gdocs-convert-test-docs-json-marked-quote-block ()
  "Marked quote-block paragraphs pull back to Org quote blocks."
  (let* ((body-elements
          (vector `((startIndex . 1)
                    (endIndex . 12)
                    (paragraph
                     . ((elements
                         . ,(vector '((textRun . ((content . "Line one\n")
                                                   (textStyle . ()))))))
                        (paragraphStyle
                         . ((namedStyleType . "NORMAL_TEXT"))))))
                  `((startIndex . 12)
                    (endIndex . 22)
                    (paragraph
                     . ((elements
                         . ,(vector '((textRun . ((content . "Line two\n")
                                                   (textStyle . ()))))))
                        (paragraphStyle
                         . ((namedStyleType . "NORMAL_TEXT"))))))))
         (json `((body . ((content . ,body-elements)))
                 (namedRanges
                  . ,(list
                      (cons (intern "gdocs-org-marker:quote-block:elem-001")
                            `((namedRanges
                               . ,(vector `((ranges
                                             . ,(vector '((startIndex . 1)
                                                          (endIndex . 11)))))))))
                      (cons (intern "gdocs-org-marker:quote-block:elem-002")
                            `((namedRanges
                               . ,(vector `((ranges
                                             . ,(vector '((startIndex . 12)
                                                          (endIndex . 21)))))))))))))
         (ir (gdocs-convert-docs-json-to-ir json))
         (block (car ir))
         (org (gdocs-convert-ir-to-org ir)))
    (should (= (length ir) 1))
    (should (eq (plist-get block :type) 'quote-block))
    (should (equal (mapcar #'gdocs-convert--runs-to-plain-text
                           (plist-get block :paragraphs))
                   '("Line one" "Line two")))
    (should (= (plist-get block :doc-start) 1))
    (should (= (plist-get block :doc-end) 22))
    (should (string= (s-trim org)
                     "#+BEGIN_QUOTE\nLine one\n\nLine two\n#+END_QUOTE"))))

(ert-deftest gdocs-convert-test-docs-json-native-footnotes ()
  "Native Google Docs footnotes convert to Org references and definitions."
  (let* ((body-elements
          (vector `((paragraph
                     . ((elements
                         . ,(vector '((textRun . ((content . "Text")
                                                   (textStyle . ()))))
                                   '((footnoteReference
                                      . ((footnoteId . "fn-a")
                                         (footnoteNumber . "1")
                                         (textStyle . ()))))
                                   '((textRun . ((content . ".\n")
                                                 (textStyle . ()))))))
                        (paragraphStyle
                         . ((namedStyleType . "NORMAL_TEXT"))))))))
         (footnote-content
          (vector `((paragraph
                     . ((elements
                         . ,(vector '((textRun . ((content . "Body.\n")
                                                   (textStyle . ()))))))
                        (paragraphStyle
                         . ((namedStyleType . "NORMAL_TEXT"))))))))
         (json `((body . ((content . ,body-elements)))
                 (footnotes
                  . ((fn-a . ((footnoteId . "fn-a")
                              (content . ,footnote-content)))))))
         (ir (gdocs-convert-docs-json-to-ir json))
         (paragraph (car ir))
         (footnote (cadr ir)))
    (should (equal (gdocs-convert--runs-to-org
                    (plist-get paragraph :contents))
                   "Text[fn:1]."))
    (should (eq (plist-get footnote :type) 'footnote))
    (should (equal (plist-get footnote :label) "1"))
    (should (equal (gdocs-convert--ir-footnote-to-org footnote)
                   "[fn:1] Body."))))

(ert-deftest gdocs-convert-test-ir-to-org-collects-footnotes-section ()
  "IR footnotes render once under a conventional Footnotes section."
  (let* ((ir (list (list :type 'paragraph
                         :style 'normal
                         :contents (list (gdocs-convert--make-plain-run "Text")
                                         (plist-put
                                          (gdocs-convert--make-plain-run "")
                                          :footnote-label "1")
                                         (gdocs-convert--make-plain-run ".")))
                   (list :type 'footnote
                         :label "1"
                         :contents (list (gdocs-convert--make-plain-run "Body.")))))
         (org (gdocs-convert-ir-to-org ir)))
    (should (equal org "Text[fn:1].\n\n* Footnotes\n\n[fn:1] Body.\n"))))

(ert-deftest gdocs-convert-test-docs-json-bold ()
  "Bold text in Google Docs JSON converts to a run with :bold t."
  (let* ((json `((body . ((content
                           . [((paragraph
                                . ((elements
                                    . [((textRun
                                         . ((content . "bold text\n")
                                            (textStyle
                                             . ((bold . t))))))])
                                   (paragraphStyle
                                    . ((namedStyleType
                                        . "NORMAL_TEXT"))))))])))))
         (ir (gdocs-convert-docs-json-to-ir json))
         (run (car (plist-get (car ir) :contents))))
    (should (plist-get run :bold))))

(ert-deftest gdocs-convert-test-docs-json-table ()
  "A Google Docs table converts to an IR table element."
  (let* ((json `((body
                  . ((content
                      . [((table
                           . ((rows . 1)
                              (columns . 2)
                              (tableRows
                               . [((tableCells
                                    . [((content
                                         . [((paragraph
                                              . ((elements
                                                  . [((textRun
                                                       . ((content . "A\n")
                                                          (textStyle))))]))))]))
                                       ((content
                                         . [((paragraph
                                              . ((elements
                                                  . [((textRun
                                                       . ((content . "B\n")
                                                          (textStyle))))]))))]))]))]))))])))))
         (ir (gdocs-convert-docs-json-to-ir json))
         (table-elem (gdocs-convert-test--find-element ir 'table)))
    (should table-elem)
    (should (= (length (plist-get table-elem :rows)) 1))
    (should (= (length (car (plist-get table-elem :rows))) 2))))

;; ---------------------------------------------------------------------------
;;; IR -> batchUpdate requests

(ert-deftest gdocs-convert-test-ir-to-requests-basic ()
  "A simple paragraph produces insertText and updateParagraphStyle requests."
  (let* ((ir (list (list :type 'paragraph
                         :style 'heading-1
                         :contents (list (gdocs-convert--make-plain-run
                                         "Hello"))
                         :id "elem-001")))
         (requests (gdocs-convert-ir-to-docs-requests ir)))
    (should (>= (length requests) 1))
    (should (alist-get 'insertText (car requests)))))

(ert-deftest gdocs-convert-test-ir-to-requests-indices ()
  "Multiple paragraphs produce requests with advancing indices."
  (let* ((ir (list (list :type 'paragraph
                         :style 'normal
                         :contents (list (gdocs-convert--make-plain-run "AB"))
                         :id "elem-001")
                   (list :type 'paragraph
                         :style 'normal
                         :contents (list (gdocs-convert--make-plain-run "CD"))
                         :id "elem-002")))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (first-insert (alist-get 'insertText (car requests)))
         (second-insert-req (--first (alist-get 'insertText it)
                                     (cdr requests)))
         (second-insert (alist-get 'insertText second-insert-req)))
    (should (= (alist-get 'index (alist-get 'location first-insert)) 1))
    (should (= (alist-get 'index (alist-get 'location second-insert)) 4))))

;; ---------------------------------------------------------------------------
;;; UTF-16 length

(ert-deftest gdocs-convert-test-utf16-ascii ()
  "ASCII characters are 1 UTF-16 code unit each."
  (should (= (gdocs-convert--string-to-utf16-length "hello") 5)))

(ert-deftest gdocs-convert-test-utf16-bmp ()
  "BMP characters (e.g., accented Latin) are 1 UTF-16 code unit each."
  (should (= (gdocs-convert--string-to-utf16-length "caf\u00e9") 4)))

(ert-deftest gdocs-convert-test-utf16-supplementary ()
  "Supplementary characters (emoji) require 2 UTF-16 code units."
  (should (= (gdocs-convert--string-to-utf16-length "\U0001F600") 2)))

(ert-deftest gdocs-convert-test-utf16-mixed ()
  "Mixed ASCII and supplementary characters compute correctly."
  (should (= (gdocs-convert--string-to-utf16-length "a\U0001F600b") 4)))

;; ---------------------------------------------------------------------------
;;; Org-only marker preservation

(ert-deftest gdocs-convert-test-todo-marker ()
  "TODO keywords are stored in :gdocs-marker as a list of plists."
  (let* ((ir (gdocs-convert-org-string-to-ir "* TODO Task"))
         (elem (car ir))
         (marker (plist-get elem :gdocs-marker)))
    (should marker)
    (should (listp marker))
    (should (eq (plist-get (car marker) :type) 'todo))
    (should (string= (plist-get (car marker) :data) "TODO"))))

(ert-deftest gdocs-convert-test-tags-marker ()
  "Tags are stored in :gdocs-marker as a list of plists."
  (let* ((ir (gdocs-convert-org-string-to-ir "* Heading :tag1:tag2:"))
         (elem (car ir))
         (marker (plist-get elem :gdocs-marker)))
    (should marker)
    (should (listp marker))
    (should (eq (plist-get (car marker) :type) 'tags))
    (should (string= (plist-get (car marker) :data) "tag1:tag2"))))

(ert-deftest gdocs-convert-test-todo-round-trip ()
  "TODO keywords survive round-trip via markers."
  (let* ((original "* TODO Important task\n")
         (ir (gdocs-convert-org-string-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    (should (string-match-p "\\* TODO Important task" result))))

(ert-deftest gdocs-convert-test-tags-round-trip ()
  "Tags survive round-trip via markers."
  (let* ((original "* Heading :work:urgent:\n")
         (ir (gdocs-convert-org-string-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    (should (string-match-p ":work:urgent:" result))))

;; ---------------------------------------------------------------------------
;;; Source block markers

(ert-deftest gdocs-convert-test-src-block-marker ()
  "A src block produces semantic source-block IR with language metadata."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "#+BEGIN_SRC python\nprint(\"hi\")\n#+END_SRC"))
         (elem (car ir)))
    (should (eq (plist-get elem :type) 'source-block))
    (should (string= (plist-get elem :language) "python"))
    (should (equal (plist-get elem :lines) '("print(\"hi\")")))))

;; ---------------------------------------------------------------------------
;;; Priority markers

(ert-deftest gdocs-convert-test-priority-marker ()
  "A heading with priority produces a marker with :type priority."
  (let* ((ir (gdocs-convert-org-string-to-ir "* [#A] Urgent task"))
         (elem (car ir))
         (marker (plist-get elem :gdocs-marker)))
    (should marker)
    (should (listp marker))
    (should (eq (plist-get (car marker) :type) 'priority))
    (should (string= (plist-get (car marker) :data) "A"))))

;; ---------------------------------------------------------------------------
;;; Scheduled/Deadline markers

(ert-deftest gdocs-convert-test-scheduled-marker ()
  "SCHEDULED produces a marker with :type scheduled."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "* Task\nSCHEDULED: <2026-03-15>"))
         (elem (car ir))
         (marker (plist-get elem :gdocs-marker)))
    (should marker)
    (should (listp marker))
    (should (eq (plist-get (car marker) :type) 'scheduled))
    (should (string-match-p "2026-03-15" (plist-get (car marker) :data)))))

(ert-deftest gdocs-convert-test-deadline-marker ()
  "DEADLINE produces a marker with :type deadline."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "* Task\nDEADLINE: <2026-03-20>"))
         (elem (car ir))
         (marker (plist-get elem :gdocs-marker)))
    (should marker)
    (should (listp marker))
    (should (eq (plist-get (car marker) :type) 'deadline))
    (should (string-match-p "2026-03-20" (plist-get (car marker) :data)))))

;; ---------------------------------------------------------------------------
;;; Empty paragraph handling

(ert-deftest gdocs-convert-test-empty-paragraph-between-paragraphs ()
  "A blank line between paragraphs does not create a whitespace-only element."
  (let* ((ir (gdocs-convert-org-string-to-ir "First paragraph\n\nSecond paragraph")))
    (should (= (length ir) 2))
    (should (string= (plist-get (car (plist-get (nth 0 ir) :contents)) :text)
                      "First paragraph"))
    (should (string= (plist-get (car (plist-get (nth 1 ir) :contents)) :text)
                      "Second paragraph"))))

;; ---------------------------------------------------------------------------
;;; Segmented parsing

(ert-deftest gdocs-convert-test-segmented-parsing ()
  "Segmented parsing returns :ir, :segments, :preamble, and :postamble."
  (with-temp-buffer
    (insert "* Heading\n\nParagraph text\n")
    (org-mode)
    (let ((result (gdocs-convert-org-buffer-to-segments)))
      (should (plist-get result :ir))
      (should (plist-get result :segments))
      (should (stringp (plist-get result :preamble)))
      (should (stringp (plist-get result :postamble)))
      ;; Segments should correspond to IR elements
      (should (= (length (plist-get result :segments))
                 (length (plist-get result :ir)))))))

;; ---------------------------------------------------------------------------
;;; Multiple formatting runs

(ert-deftest gdocs-convert-test-multiple-formatting-runs ()
  "*bold* and /italic/ mixed produces correct run sequence."
  (let* ((ir (gdocs-convert-org-string-to-ir "*bold* and /italic/ mixed"))
         (runs (plist-get (car ir) :contents)))
    ;; Should have at least 3 runs: bold, plain, italic
    (should (>= (length runs) 3))
    ;; First run should be bold
    (let ((bold-run (car runs)))
      (should (plist-get bold-run :bold))
      (should (string= (plist-get bold-run :text) "bold")))
    ;; There should be an italic run
    (let ((italic-run (--first (plist-get it :italic) runs)))
      (should italic-run)
      (should (string= (plist-get italic-run :text) "italic")))))

;; ---------------------------------------------------------------------------
;;; Keyword markers

(ert-deftest gdocs-convert-test-keyword-marker ()
  "#+AUTHOR: Pablo produces an IR element with :gdocs-marker of :type keyword."
  (let* ((ir (gdocs-convert-org-string-to-ir "#+AUTHOR: Pablo"))
         (elem (--first (let ((m (plist-get it :gdocs-marker)))
                          (and m (eq (plist-get (car m) :type) 'keyword)))
                        ir))
         (marker (plist-get elem :gdocs-marker)))
    (should marker)
    (should (listp marker))
    (should (eq (plist-get (car marker) :type) 'keyword))
    (should (string= (plist-get (plist-get (car marker) :data) :key) "AUTHOR"))
    (should (string= (plist-get (plist-get (car marker) :data) :value) "Pablo"))))

;; ---------------------------------------------------------------------------
;;; gdocs-convert--runs-to-plain-text

(ert-deftest gdocs-convert-test-runs-to-plain-text ()
  "Concatenates :text values from multiple runs."
  (let ((runs (list (list :text "Hello" :bold nil)
                    (list :text " " :bold nil)
                    (list :text "world" :bold t))))
    (should (string= (gdocs-convert--runs-to-plain-text runs) "Hello world"))))

(ert-deftest gdocs-convert-test-runs-to-plain-text-empty ()
  "Empty run list produces empty string."
  (should (string= (gdocs-convert--runs-to-plain-text nil) "")))

;; ---------------------------------------------------------------------------
;;; gdocs-convert--make-plain-run

(ert-deftest gdocs-convert-test-make-plain-run ()
  "Creates a plist with :text set and all formatting nil."
  (let ((run (gdocs-convert--make-plain-run "Hello")))
    (should (string= (plist-get run :text) "Hello"))
    (should-not (plist-get run :bold))
    (should-not (plist-get run :italic))
    (should-not (plist-get run :underline))
    (should-not (plist-get run :strikethrough))
    (should-not (plist-get run :code))
    (should-not (plist-get run :link))))

;; ---------------------------------------------------------------------------
;;; Round-trip with headings and nested lists

(ert-deftest gdocs-convert-test-round-trip-headings-and-nested-lists ()
  "Headings and nested lists survive round-trip."
  (let* ((original "* Top heading\n\n- Parent item\n  - Child item\n    - Grandchild item\n\n** Sub heading\n\nParagraph.\n")
         (ir (gdocs-convert-org-string-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    (should (string-match-p "\\* Top heading" result))
    (should (string-match-p "\\*\\* Sub heading" result))
    (should (string-match-p "- Parent item" result))
    (should (string-match-p "  - Child item" result))
    (should (string-match-p "    - Grandchild item" result))
    (should (string-match-p "Paragraph\\." result))))

;; ---------------------------------------------------------------------------
;;; Property drawer reader

(ert-deftest gdocs-convert-test-read-file-property-gdocs-id ()
  "Reads GDOCS_DOCUMENT_ID from a file's property drawer."
  (let ((temp-file (make-temp-file "gdocs-test" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert ":PROPERTIES:\n"
                    ":GDOCS_DOCUMENT_ID: abc123def\n"
                    ":END:\n"
                    "* Heading\n\nSome text.\n"))
          (should (string= (gdocs-convert--read-file-property-gdocs-id temp-file)
                            "abc123def")))
      (delete-file temp-file))))

(ert-deftest gdocs-convert-test-read-file-property-gdocs-id-missing ()
  "Returns nil when file has no GDOCS_DOCUMENT_ID property."
  (let ((temp-file (make-temp-file "gdocs-test" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "* Just a heading\n"))
          (should-not (gdocs-convert--read-file-property-gdocs-id temp-file)))
      (delete-file temp-file))))

(ert-deftest gdocs-convert-test-read-file-property-gdocs-id-nonexistent ()
  "Returns nil for nonexistent files."
  (should-not (gdocs-convert--read-file-property-gdocs-id
               "/nonexistent/path/to/file.org")))

;; ---------------------------------------------------------------------------
;;; Cross-document link resolution

(ert-deftest gdocs-convert-test-link-url-web-unchanged ()
  "Web links are unchanged regardless of link context."
  (let ((gdocs-convert--link-context
         (list :buffer-file "/tmp/test.org"
               :docid-map (make-hash-table :test 'equal))))
    (let* ((ir (gdocs-convert-org-string-to-ir
                "Visit [[https://example.com][Example]]"))
           (runs (plist-get (car ir) :contents))
           (link-run (nth 1 runs)))
      (should (string= (plist-get link-run :link) "https://example.com")))))

(ert-deftest gdocs-convert-test-link-url-file-resolved ()
  "file: links to org files with doc IDs resolve to Google Docs URLs."
  (let ((temp-dir (make-temp-file "gdocs-test" t))
        (source-file nil)
        (target-file nil))
    (unwind-protect
        (progn
          (setq source-file (expand-file-name "source.org" temp-dir))
          (setq target-file (expand-file-name "target.org" temp-dir))
          (with-temp-file target-file
            (insert ":PROPERTIES:\n"
                    ":GDOCS_DOCUMENT_ID: TARGET_DOC_ID\n"
                    ":END:\n"
                    "* Target heading\n"))
          (with-temp-file source-file
            (insert (format "Link to [[file:%s][Target]]" target-file)))
          (let ((gdocs-convert--link-context
                 (list :buffer-file source-file
                       :docid-map (make-hash-table :test 'equal))))
            (with-temp-buffer
              (insert-file-contents source-file)
              (org-mode)
              (let* ((ir (gdocs-convert-org-buffer-to-ir))
                     (runs (plist-get (car ir) :contents))
                     (link-run (nth 1 runs)))
                (should (string=
                         (plist-get link-run :link)
                         "https://docs.google.com/document/d/TARGET_DOC_ID/edit"))))))
      (delete-file source-file)
      (delete-file target-file)
      (delete-directory temp-dir))))

(ert-deftest gdocs-convert-test-link-url-file-no-doc-id ()
  "file: links to org files without doc IDs become plain text."
  (let ((temp-dir (make-temp-file "gdocs-test" t))
        (source-file nil)
        (target-file nil))
    (unwind-protect
        (progn
          (setq source-file (expand-file-name "source.org" temp-dir))
          (setq target-file (expand-file-name "target.org" temp-dir))
          (with-temp-file target-file
            (insert "* Target heading\n"))
          (with-temp-file source-file
            (insert "Link to [[file:target.org][Target]]"))
          (let ((gdocs-convert--link-context
                 (list :buffer-file source-file
                       :docid-map (make-hash-table :test 'equal))))
            (with-temp-buffer
              (insert-file-contents source-file)
              (org-mode)
              (let* ((ir (gdocs-convert-org-buffer-to-ir))
                     (runs (plist-get (car ir) :contents)))
                ;; Link text merges with surrounding text as plain text
                (should (string= (plist-get (car runs) :text) "Link to Target"))
                (should-not (plist-get (car runs) :link))))))
      (delete-file source-file)
      (delete-file target-file)
      (delete-directory temp-dir))))

(ert-deftest gdocs-convert-test-link-url-no-context ()
  "Without link context, file: links return raw path (backward compat)."
  (let ((gdocs-convert--link-context nil))
    (let* ((ir (gdocs-convert-org-string-to-ir
                "Link to [[file:other.org][Other]]"))
           (runs (plist-get (car ir) :contents))
           (link-run (nth 1 runs)))
      (should (string= (plist-get link-run :link) "file:other.org")))))

(ert-deftest gdocs-convert-test-link-url-id-no-doc-id ()
  "id: links to files without doc IDs become plain text."
  (let ((temp-dir (make-temp-file "gdocs-test" t))
        (source-file nil)
        (target-file nil))
    (unwind-protect
        (progn
          (setq source-file (expand-file-name "source.org" temp-dir))
          (setq target-file (expand-file-name "target.org" temp-dir))
          (with-temp-file target-file
            (insert "* Target heading\n"
                    ":PROPERTIES:\n"
                    ":ID:       TEST-UUID-1234\n"
                    ":END:\n"))
          ;; Register the ID in org-id so org-id-find can locate it
          (let ((org-id-locations (make-hash-table :test 'equal)))
            (puthash "TEST-UUID-1234" target-file org-id-locations)
            (with-temp-file source-file
              (insert "Link to [[id:TEST-UUID-1234][Target]]"))
            (let ((gdocs-convert--link-context
                   (list :buffer-file source-file
                         :docid-map (make-hash-table :test 'equal))))
              (with-temp-buffer
                (insert-file-contents source-file)
                (org-mode)
                (let* ((ir (gdocs-convert-org-buffer-to-ir))
                       (runs (plist-get (car ir) :contents)))
                  ;; Link text merges with surrounding text as plain text
                  (should (string= (plist-get (car runs) :text) "Link to Target"))
                  (should-not (plist-get (car runs) :link)))))))
      (delete-file source-file)
      (delete-file target-file)
      (delete-directory temp-dir))))

(ert-deftest gdocs-convert-test-link-url-id-no-context ()
  "Without link context, id: links preserve id: prefix."
  (let ((gdocs-convert--link-context nil))
    (let* ((ir (gdocs-convert-org-string-to-ir
                "Link to [[id:SOME-UUID][Other]]"))
           (runs (plist-get (car ir) :contents))
           (link-run (nth 1 runs)))
      (should (string= (plist-get link-run :link) "id:SOME-UUID")))))

;; ---------------------------------------------------------------------------
;;; Heading cache

(ert-deftest gdocs-convert-test-cache-heading-ids ()
  "Heading IDs are extracted and cached from docs JSON."
  (let ((gdocs-convert--heading-cache (make-hash-table :test 'equal))
        (gdocs-convert--heading-reverse-cache (make-hash-table :test 'equal))
        (json `((body . ((content
                          . [((paragraph
                               . ((elements
                                   . [((textRun
                                        . ((content . "Introduction\n")
                                           (textStyle . ()))))])
                                  (paragraphStyle
                                   . ((namedStyleType . "HEADING_1")
                                      (headingId . "h123abc"))))))])))
                (title . "Test"))))
    (gdocs-convert--cache-heading-ids "doc-001" json)
    (let ((forward (gethash "doc-001" gdocs-convert--heading-cache))
          (reverse (gethash "doc-001" gdocs-convert--heading-reverse-cache)))
      (should (equal forward '(("Introduction" . "h123abc"))))
      (should (equal reverse '(("h123abc" . "Introduction")))))))

(ert-deftest gdocs-convert-test-heading-anchor-appended ()
  "When heading cache is populated, links include heading anchor."
  (let ((temp-dir (make-temp-file "gdocs-test" t))
        (source-file nil)
        (target-file nil)
        (gdocs-convert--heading-cache (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (setq source-file (expand-file-name "source.org" temp-dir))
          (setq target-file (expand-file-name "target.org" temp-dir))
          (with-temp-file target-file
            (insert ":PROPERTIES:\n"
                    ":GDOCS_DOCUMENT_ID: TARGET_DOC\n"
                    ":END:\n"
                    "* Introduction\n"))
          ;; Populate heading cache
          (puthash "TARGET_DOC"
                   '(("Introduction" . "h_abc123"))
                   gdocs-convert--heading-cache)
          (with-temp-file source-file
            (insert "See [[file:target.org::*Introduction][intro]]"))
          (let ((gdocs-convert--link-context
                 (list :buffer-file source-file
                       :docid-map (make-hash-table :test 'equal))))
            (with-temp-buffer
              (insert-file-contents source-file)
              (org-mode)
              (let* ((ir (gdocs-convert-org-buffer-to-ir))
                     (runs (plist-get (car ir) :contents))
                     (link-run (nth 1 runs)))
                (should (string=
                         (plist-get link-run :link)
                         "https://docs.google.com/document/d/TARGET_DOC/edit#heading=h.h_abc123"))))))
      (delete-file source-file)
      (delete-file target-file)
      (delete-directory temp-dir))))

(ert-deftest gdocs-convert-test-no-heading-anchor-on-cache-miss ()
  "Without heading cache, link resolves to document-level URL."
  (let ((temp-dir (make-temp-file "gdocs-test" t))
        (source-file nil)
        (target-file nil)
        (gdocs-convert--heading-cache (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (setq source-file (expand-file-name "source.org" temp-dir))
          (setq target-file (expand-file-name "target.org" temp-dir))
          (with-temp-file target-file
            (insert ":PROPERTIES:\n"
                    ":GDOCS_DOCUMENT_ID: TARGET_DOC\n"
                    ":END:\n"
                    "* Introduction\n"))
          ;; No heading cache populated
          (with-temp-file source-file
            (insert "See [[file:target.org::*Introduction][intro]]"))
          (let ((gdocs-convert--link-context
                 (list :buffer-file source-file
                       :docid-map (make-hash-table :test 'equal))))
            (with-temp-buffer
              (insert-file-contents source-file)
              (org-mode)
              (let* ((ir (gdocs-convert-org-buffer-to-ir))
                     (runs (plist-get (car ir) :contents))
                     (link-run (nth 1 runs)))
                (should (string=
                         (plist-get link-run :link)
                         "https://docs.google.com/document/d/TARGET_DOC/edit"))))))
      (delete-file source-file)
      (delete-file target-file)
      (delete-directory temp-dir))))

;; ---------------------------------------------------------------------------
;;; Reverse resolution

(ert-deftest gdocs-convert-test-reverse-resolve-google-docs-url ()
  "Google Docs URLs reverse-resolve to file: links on pull."
  (let* ((docid-map (make-hash-table :test 'equal))
         (gdocs-convert--heading-cache (make-hash-table :test 'equal))
         (gdocs-convert--link-context
          (list :buffer-file "/home/user/org/source.org"
                :docid-map docid-map)))
    (puthash "DOC_ABC" "/home/user/org/target.org" docid-map)
    (let* ((run (list :text "link text"
                      :bold nil :italic nil :underline nil
                      :strikethrough nil :code nil
                      :link "https://docs.google.com/document/d/DOC_ABC/edit"))
           (org-text (gdocs-convert--run-to-org run)))
      (should (string= org-text "[[file:target.org][link text]]")))))

(ert-deftest gdocs-convert-test-reverse-resolve-with-heading ()
  "Google Docs URLs with heading anchors reverse-resolve with ::*Heading."
  (let* ((docid-map (make-hash-table :test 'equal))
         (gdocs-convert--heading-cache (make-hash-table :test 'equal))
         (gdocs-convert--heading-reverse-cache (make-hash-table :test 'equal))
         (gdocs-convert--link-context
          (list :buffer-file "/home/user/org/source.org"
                :docid-map docid-map)))
    (puthash "DOC_ABC" "/home/user/org/target.org" docid-map)
    (puthash "DOC_ABC" '(("h_intro" . "Introduction"))
             gdocs-convert--heading-reverse-cache)
    (let* ((run (list :text "intro"
                      :bold nil :italic nil :underline nil
                      :strikethrough nil :code nil
                      :link "https://docs.google.com/document/d/DOC_ABC/edit#heading=h.h_intro"))
           (org-text (gdocs-convert--run-to-org run)))
      (should (string= org-text
                        "[[file:target.org::*Introduction][intro]]")))))

(ert-deftest gdocs-convert-test-reverse-resolve-unknown-doc ()
  "Unknown Google Docs URLs pass through unchanged."
  (let* ((docid-map (make-hash-table :test 'equal))
         (gdocs-convert--link-context
          (list :buffer-file "/home/user/org/source.org"
                :docid-map docid-map)))
    (let* ((run (list :text "unknown"
                      :bold nil :italic nil :underline nil
                      :strikethrough nil :code nil
                      :link "https://docs.google.com/document/d/UNKNOWN_DOC/edit"))
           (org-text (gdocs-convert--run-to-org run)))
      (should (string=
               org-text
               "[[https://docs.google.com/document/d/UNKNOWN_DOC/edit][unknown]]")))))

(ert-deftest gdocs-convert-test-reverse-resolve-non-docs-url ()
  "Non-Google Docs URLs pass through unchanged."
  (let* ((gdocs-convert--link-context
          (list :buffer-file "/home/user/org/source.org"
                :docid-map (make-hash-table :test 'equal))))
    (let* ((run (list :text "example"
                      :bold nil :italic nil :underline nil
                      :strikethrough nil :code nil
                      :link "https://example.com"))
           (org-text (gdocs-convert--run-to-org run)))
      (should (string= org-text "[[https://example.com][example]]")))))

;; ---------------------------------------------------------------------------
;;; Build docid-to-file map

(ert-deftest gdocs-convert-test-build-docid-map ()
  "Builds a hash table mapping doc IDs to file paths."
  (let ((temp-dir (make-temp-file "gdocs-test" t))
        (file-a nil)
        (file-b nil))
    (unwind-protect
        (progn
          (setq file-a (expand-file-name "a.org" temp-dir))
          (setq file-b (expand-file-name "b.org" temp-dir))
          (with-temp-file file-a
            (insert ":PROPERTIES:\n"
                    ":GDOCS_DOCUMENT_ID: DOC_A\n"
                    ":END:\n"
                    "* A\n"))
          (with-temp-file file-b
            (insert ":PROPERTIES:\n"
                    ":GDOCS_DOCUMENT_ID: DOC_B\n"
                    ":END:\n"
                    "* B\n"))
          (let ((map (gdocs-convert--build-docid-to-file-map
                      (list temp-dir))))
            (should (string= (gethash "DOC_A" map) file-a))
            (should (string= (gethash "DOC_B" map) file-b))))
      (delete-file file-a)
      (delete-file file-b)
      (delete-directory temp-dir))))

;; ---------------------------------------------------------------------------
;;; Integration: round-trip with cross-document links

(ert-deftest gdocs-convert-test-cross-doc-link-round-trip ()
  "File link -> IR (resolved URL) -> org (file: link) round-trip."
  (let* ((temp-dir (make-temp-file "gdocs-test" t))
         (source-file (expand-file-name "source.org" temp-dir))
         (target-file (expand-file-name "target.org" temp-dir))
         (docid-map (make-hash-table :test 'equal))
         (gdocs-convert--heading-cache (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file target-file
            (insert ":PROPERTIES:\n"
                    ":GDOCS_DOCUMENT_ID: DOC_TARGET\n"
                    ":END:\n"
                    "* Target\n"))
          (with-temp-file source-file
            (insert "See [[file:target.org][Target doc]]"))
          ;; Build docid-map for reverse resolution
          (puthash "DOC_TARGET" target-file docid-map)
          (let ((gdocs-convert--link-context
                 (list :buffer-file source-file
                       :docid-map docid-map)))
            ;; org -> IR: file link becomes Google Docs URL
            (let* ((ir (with-temp-buffer
                         (insert-file-contents source-file)
                         (org-mode)
                         (gdocs-convert-org-buffer-to-ir)))
                   (link-run (nth 1 (plist-get (car ir) :contents))))
              (should (string=
                       (plist-get link-run :link)
                       "https://docs.google.com/document/d/DOC_TARGET/edit"))
              ;; IR -> org: Google Docs URL becomes file: link
              (let ((result (gdocs-convert-ir-to-org ir)))
                (should (string-match-p "\\[\\[file:target\\.org\\]" result))))))
      (delete-file source-file)
      (delete-file target-file)
      (delete-directory temp-dir))))

;; ---------------------------------------------------------------------------
;;; Internal heading links (headingId)

(ert-deftest gdocs-convert-test-heading-id-to-url ()
  "Internal headingId links are converted to full Google Docs URLs."
  (let* ((gdocs-convert--document-id "DOC_SELF")
         (json `((body . ((content
                           . [((paragraph
                                . ((elements
                                    . [((textRun
                                         . ((content . "click here\n")
                                            (textStyle
                                             . ((link
                                                 . ((headingId . "h_intro"))))))))])
                                   (paragraphStyle
                                    . ((namedStyleType . "NORMAL_TEXT"))))))])))))
         (ir (gdocs-convert-docs-json-to-ir json))
         (runs (plist-get (car ir) :contents))
         (link-run (car runs)))
    (should (string=
             (plist-get link-run :link)
             "https://docs.google.com/document/d/DOC_SELF/edit#heading=h.h_intro"))))

(ert-deftest gdocs-convert-test-heading-id-without-doc-id ()
  "Without `gdocs-convert--document-id', headingId links are nil."
  (let* ((gdocs-convert--document-id nil)
         (text-run `((content . "click\n")
                     (textStyle . ((link . ((headingId . "h_intro")))))))
         (ir-run (gdocs-convert--docs-text-run-to-ir text-run)))
    (should (null (plist-get ir-run :link)))))

;; ---------------------------------------------------------------------------
;;; Same-document reverse resolution

(ert-deftest gdocs-convert-test-reverse-resolve-same-doc-with-org-id ()
  "Same-document heading links resolve to id:UUID when heading has an ID."
  (let* ((temp-dir (make-temp-file "gdocs-test" t))
         (doc-file (expand-file-name "doc.org" temp-dir))
         (docid-map (make-hash-table :test 'equal))
         (gdocs-convert--heading-cache (make-hash-table :test 'equal))
         (gdocs-convert--heading-reverse-cache (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file doc-file
            (insert "* Building a pipeline\n"
                    ":PROPERTIES:\n"
                    ":ID:       DE2746D4-0EE2-4D37-9CCE-7264E953EB90\n"
                    ":END:\n"))
          (puthash "DOC_SELF" doc-file docid-map)
          (puthash "DOC_SELF"
                   '(("h_build" . "Building a pipeline"))
                   gdocs-convert--heading-reverse-cache)
          (let* ((gdocs-convert--link-context
                  (list :buffer-file doc-file
                        :docid-map docid-map))
                 (run (list :text "more"
                            :bold nil :italic nil :underline nil
                            :strikethrough nil :code nil
                            :link "https://docs.google.com/document/d/DOC_SELF/edit#heading=h.h_build"))
                 (org-text (gdocs-convert--run-to-org run)))
            (should (string= org-text
                              "[[id:DE2746D4-0EE2-4D37-9CCE-7264E953EB90][more]]"))))
      (delete-file doc-file)
      (delete-directory temp-dir))))

(ert-deftest gdocs-convert-test-reverse-resolve-same-doc-no-org-id ()
  "Same-document heading links fall back to *Heading without an ID."
  (let* ((temp-dir (make-temp-file "gdocs-test" t))
         (doc-file (expand-file-name "doc.org" temp-dir))
         (docid-map (make-hash-table :test 'equal))
         (gdocs-convert--heading-cache (make-hash-table :test 'equal))
         (gdocs-convert--heading-reverse-cache (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file doc-file
            (insert "* Building a pipeline\n"))
          (puthash "DOC_SELF" doc-file docid-map)
          (puthash "DOC_SELF"
                   '(("h_build" . "Building a pipeline"))
                   gdocs-convert--heading-reverse-cache)
          (let* ((gdocs-convert--link-context
                  (list :buffer-file doc-file
                        :docid-map docid-map))
                 (run (list :text "more"
                            :bold nil :italic nil :underline nil
                            :strikethrough nil :code nil
                            :link "https://docs.google.com/document/d/DOC_SELF/edit#heading=h.h_build"))
                 (org-text (gdocs-convert--run-to-org run)))
            (should (string= org-text
                              "[[*Building a pipeline][more]]"))))
      (delete-file doc-file)
      (delete-directory temp-dir))))

(ert-deftest gdocs-convert-test-reverse-resolve-same-doc-no-heading ()
  "Same-document links without heading anchor produce file: link."
  (let* ((docid-map (make-hash-table :test 'equal))
         (gdocs-convert--link-context
          (list :buffer-file "/home/user/org/doc.org"
                :docid-map docid-map)))
    (puthash "DOC_SELF" "/home/user/org/doc.org" docid-map)
    (let* ((url "https://docs.google.com/document/d/DOC_SELF/edit")
           (result (gdocs-convert--reverse-resolve-link url)))
      (should (string= result "file:doc.org")))))

;;;; Guard non-list paragraphs between list groups

(ert-deftest gdocs-convert-test-guard-non-list-paragraphs ()
  "Headings between list groups get deleteParagraphBullets."
  (let* ((heading (list :type 'paragraph :style 'heading-3
                        :contents (list (list :text "Heading"))
                        :id "h1"))
         (item1 (list :type 'paragraph :style 'normal
                      :contents (list (list :text "Item 1"))
                      :list (list :type 'number :level 0)
                      :id "i1"))
         (item2 (list :type 'paragraph :style 'normal
                      :contents (list (list :text "Item 2"))
                      :list (list :type 'number :level 0)
                      :id "i2"))
         ;; Simulate element-ranges: item1=[1,8), heading=[8,16), item2=[16,23)
         (element-ranges (list (list :element item1 :start 1 :end 8)
                               (list :element heading :start 8 :end 16)
                               (list :element item2 :start 16 :end 23)))
         ;; Two groups: [1,8) and [16,23)
         (groups (list (list :start 1 :end 8
                             :preset "NUMBERED_DECIMAL_ALPHA_ROMAN")
                       (list :start 16 :end 23
                             :preset "NUMBERED_DECIMAL_NESTED")))
         (result (gdocs-convert--guard-non-list-paragraphs
                  element-ranges groups)))
    ;; Should produce one deleteParagraphBullets for the heading
    (should (= (length result) 1))
    (let* ((req (car result))
           (range (alist-get 'range
                             (alist-get 'deleteParagraphBullets req))))
      (should (= (alist-get 'startIndex range) 8))
      (should (= (alist-get 'endIndex range) 15)))))

(ert-deftest gdocs-convert-test-guard-no-request-for-list-item ()
  "List items between groups do not get deleteParagraphBullets."
  (let* ((item-between (list :type 'paragraph :style 'normal
                             :contents (list (list :text "X"))
                             :list (list :type 'bullet :level 0)
                             :id "ib"))
         (element-ranges (list (list :element item-between
                                     :start 8 :end 10)))
         (groups (list (list :start 1 :end 8
                             :preset "NUMBERED_DECIMAL_ALPHA_ROMAN")
                       (list :start 10 :end 17
                             :preset "NUMBERED_DECIMAL_NESTED")))
         (result (gdocs-convert--guard-non-list-paragraphs
                  element-ranges groups)))
    (should (null result))))

;; ---------------------------------------------------------------------------
;;; Empty/minimal inputs

(ert-deftest gdocs-convert-test-empty-buffer-to-ir ()
  "An empty buffer returns an empty IR list."
  (let ((ir (gdocs-convert-org-string-to-ir "")))
    (should (null ir))))

(ert-deftest gdocs-convert-test-title-only-to-ir ()
  "A buffer with just #+TITLE returns one title element."
  (let ((ir (gdocs-convert-org-string-to-ir "#+TITLE: My doc")))
    (should (= (length ir) 1))
    (should (eq (plist-get (car ir) :type) 'paragraph))
    (should (eq (plist-get (car ir) :style) 'title))
    (should (string= (plist-get (gdocs-convert-test--first-content-run ir)
                                :text)
                      "My doc"))))

(ert-deftest gdocs-convert-test-whitespace-only-to-ir ()
  "A buffer with only whitespace returns an empty IR list."
  (let ((ir (gdocs-convert-org-string-to-ir "   \n\n  \n")))
    (should (null ir))))

;; ---------------------------------------------------------------------------
;;; Image conversion

(ert-deftest gdocs-convert-test-standalone-image-link-to-ir ()
  "An undescribed standalone local image link becomes image IR."
  (let* ((ir (gdocs-convert-org-string-to-ir "[[file:img/logo.png]]\n"))
         (image (car ir)))
    (should (= (length ir) 1))
    (should (eq (plist-get image :type) 'image))
    (should (equal (plist-get image :path) "img/logo.png"))))

(ert-deftest gdocs-convert-test-captioned-image-link-to-ir ()
  "An Org caption before a standalone image is captured in image IR."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "#+CAPTION: Product logo\n[[file:img/logo.png]]\n"))
         (image (car ir)))
    (should (eq (plist-get image :type) 'image))
    (should (equal (plist-get image :caption) "Product logo"))))

(ert-deftest gdocs-convert-test-described-image-link-stays-link ()
  "A described local image link remains a normal link, not image IR."
  (let* ((ir (gdocs-convert-org-string-to-ir "[[file:img/logo.png][Open image]]\n"))
         (paragraph (car ir)))
    (should (eq (plist-get paragraph :type) 'paragraph))
    (should (equal (gdocs-convert--runs-to-org (plist-get paragraph :contents))
                   "[[file:img/logo.png][Open image]]"))))

(ert-deftest gdocs-convert-test-image-ir-to-org ()
  "Image IR renders back to an Org image link and optional caption."
  (let ((org (gdocs-convert-ir-to-org
              (list (list :type 'image
                          :path "img/logo.png"
                          :caption "Product logo")))))
    (should (equal org "#+CAPTION: Product logo\n[[file:img/logo.png]]\n"))))

(ert-deftest gdocs-convert-test-remote-image-ir-to-org ()
  "Remote image IR renders back to an Org URL link."
  (let ((org (gdocs-convert-ir-to-org
              (list (list :type 'image
                          :source-uri "https://example.invalid/logo.png")))))
    (should (equal org "[[https://example.invalid/logo.png]]\n"))))

(ert-deftest gdocs-convert-test-image-block-spacing ()
  "Image IR is separated from surrounding paragraphs by blank lines."
  (let ((org (gdocs-convert-ir-to-org
              (list (list :type 'paragraph
                          :style 'normal
                          :contents (list (list :text "Before.")))
                    (list :type 'image
                          :source-uri "https://example.invalid/logo.png"
                          :caption "Product logo")
                    (list :type 'paragraph
                          :style 'normal
                          :contents (list (list :text "After.")))))))
    (should (equal org
                   "Before.\n\n#+CAPTION: Product logo\n[[https://example.invalid/logo.png]]\n\nAfter.\n"))))

(ert-deftest gdocs-convert-test-image-ir-to-requests ()
  "Image IR with a fetchable URI emits an insertInlineImage request."
  (let* ((result (gdocs-convert--image-to-requests
                  (list :type 'image :uri "https://example.invalid/logo.png")
                  12))
         (requests (plist-get result :requests)))
    (should (= (plist-get result :index) 13))
    (should (equal requests
                   '(((insertInlineImage
                       . ((uri . "https://example.invalid/logo.png")
                          (location . ((index . 12)))))))))))

(ert-deftest gdocs-convert-test-captioned-image-ir-to-requests ()
  "Captioned image IR emits inline image plus visible caption text."
  (let* ((result (gdocs-convert--image-to-requests
                  (list :type 'image
                        :uri "https://example.invalid/logo.png"
                        :caption "Product logo")
                  12))
         (requests (plist-get result :requests)))
    (should (= (plist-get result :index) 27))
    (should (member '((insertInlineImage
                       . ((uri . "https://example.invalid/logo.png")
                          (location . ((index . 12))))))
                    requests))
    (should (member '((insertText . ((text . "\nProduct logo\n")
                                     (location . ((index . 13))))))
                    requests))
    (should (member '((updateParagraphStyle
                       . ((paragraphStyle . ((namedStyleType . "NORMAL_TEXT")))
                          (range . ((startIndex . 13)
                                    (endIndex . 26)))
                          (fields . "namedStyleType"))))
                    requests))
    (should (member '((createNamedRange
                       . ((name . "gdocs-org-marker:image-caption:image-14:org-image-caption")
                          (range . ((startIndex . 14)
                                    (endIndex . 26))))))
                    requests))))

(ert-deftest gdocs-convert-test-paragraph-border-padding-emits-border-requests ()
  "Paragraph border padding resolves to complete transparent border objects."
  (let* ((gdocs-style-definitions
          '((gdocs-quote-block-single
             :parent normal
             :paragraph (:background-color "#f5f5f5"
                         :border-padding 6))))
         (request (gdocs-convert--logical-style-paragraph-request
                   'gdocs-quote-block-single 1 10))
         (update (alist-get 'updateParagraphStyle request))
         (style (alist-get 'paragraphStyle update)))
    (should (alist-get 'borderLeft style))
    (should (equal (alist-get 'width (alist-get 'borderLeft style))
                   '((magnitude . 1) (unit . "PT"))))
    (should (equal (alist-get 'color (alist-get 'borderLeft style))
                   (gdocs-convert--hex-to-color "#f5f5f5")))
    (should (equal (alist-get 'fields update)
                   "namedStyleType,shading.backgroundColor,borderTop,borderBottom,borderLeft,borderRight"))))

(ert-deftest gdocs-convert-test-paragraph-shading-uses-nested-field-mask ()
  "Paragraph shading style requests use the nested Google Docs field mask."
  (let* ((gdocs-style-definitions
          '((gdocs-source-block-single
             :parent normal
             :paragraph (:background-color "#f5f5f5"))))
         (request (gdocs-convert--logical-style-paragraph-request
                   'gdocs-source-block-single 1 10))
         (update (alist-get 'updateParagraphStyle request)))
    (should (string-match-p "shading\\.backgroundColor"
                            (alist-get 'fields update)))))

(ert-deftest gdocs-convert-test-image-caption-style-policy-is-overridable ()
  "Logical image caption style can be changed by style definitions."
  (let* ((gdocs-style-definitions
          '((gdocs-image-caption
             :parent normal
             :paragraph (:alignment center)
             :text (:italic t :font-scale 0.8 :foreground-color "#666666"))))
         (result (gdocs-convert--image-to-requests
                  (list :type 'image
                        :uri "https://example.invalid/logo.png"
                        :caption "Product logo")
                  12))
         (requests (plist-get result :requests)))
    (should (member '((updateParagraphStyle
                       . ((paragraphStyle . ((namedStyleType . "NORMAL_TEXT")
                                             (alignment . "CENTER")))
                          (range . ((startIndex . 13)
                                    (endIndex . 26)))
                          (fields . "namedStyleType,alignment"))))
                    requests))
    (should (seq-some
             (lambda (request)
               (let ((style (alist-get 'textStyle
                                        (alist-get 'updateTextStyle request))))
                 (and (equal (alist-get 'italic style) t)
                      (alist-get 'fontSize style)
                      (alist-get 'foregroundColor style))))
             requests))))

(ert-deftest gdocs-convert-test-image-ir-without-uri-noops ()
  "Image IR without a fetchable URI is left to adapter preflight."
  (let ((result (gdocs-convert--image-to-requests
                 (list :type 'image :path "img/logo.png")
                 12)))
    (should-not (plist-get result :requests))
    (should (= (plist-get result :index) 12))))

;; ---------------------------------------------------------------------------
;;; Callout block conversion

(ert-deftest gdocs-convert-test-callout-block-to-ir ()
  "Canonical CALLOUT special blocks produce semantic callout IR."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "#+ATTR_CALLOUT: :type warning :title \"Heads up\"\n#+BEGIN_CALLOUT\nBody with *bold*.\n#+END_CALLOUT"))
         (block (car ir)))
    (should (eq (plist-get block :type) 'callout-block))
    (should (equal (plist-get block :callout-type) "warning"))
    (should (equal (plist-get block :title) "Heads up"))
    (should (equal (mapcar #'gdocs-convert--runs-to-plain-text
                           (plist-get block :paragraphs))
                   '("Body with bold.")))))

(ert-deftest gdocs-convert-test-callout-defaults-to-info ()
  "Canonical CALLOUT blocks default to info type."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "#+BEGIN_CALLOUT\nBody.\n#+END_CALLOUT"))
         (block (car ir)))
    (should (equal (plist-get block :callout-type) "info"))))

(ert-deftest gdocs-convert-test-callout-rejects-unsupported-type ()
  "Canonical CALLOUT blocks fail closed for unsupported types."
  (should-error
   (gdocs-convert-org-string-to-ir
    "#+ATTR_CALLOUT: :type danger\n#+BEGIN_CALLOUT\nBody.\n#+END_CALLOUT")
   :type 'user-error))

(ert-deftest gdocs-convert-test-callout-block-preserves-unordered-list ()
  "Canonical CALLOUT blocks preserve unordered list body elements."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "#+ATTR_CALLOUT: :type tip\n#+BEGIN_CALLOUT\n- one\n- two\n#+END_CALLOUT"))
         (block (car ir))
         (body-elements (plist-get block :body-elements))
         (org (gdocs-convert-ir-to-org ir)))
    (should (= (length ir) 1))
    (should (eq (plist-get block :type) 'callout-block))
    (should (= (length body-elements) 2))
    (should (equal (plist-get (car body-elements) :list)
                   '(:type bullet :level 0)))
    (should (string-match-p "^- one\n- two$" org))))

(ert-deftest gdocs-convert-test-callout-rejects-nested-table ()
  "Canonical CALLOUT v1 fails closed for unsupported nested tables."
  (should-error
   (gdocs-convert-org-string-to-ir
    "#+BEGIN_CALLOUT\n| one |\n#+END_CALLOUT")
   :type 'user-error))

(ert-deftest gdocs-convert-test-callout-block-requests-mark-list-body ()
  "Callout list body requests keep native bullets and reset indentation."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "#+ATTR_CALLOUT: :type tip\n#+BEGIN_CALLOUT\n- one\n- two\n#+END_CALLOUT"))
         (requests (gdocs-convert-ir-to-docs-requests ir)))
    (should (seq-some (lambda (request)
                        (let ((range (alist-get 'createNamedRange request)))
                          (and range
                               (string-prefix-p
                                "gdocs-org-marker:callout-body:elem-003:tip:"
                                (alist-get 'name range)))))
                      requests))
    (should (member '((insertText . ((text . "one\n")
                                     (location . ((index . 5))))))
                    requests))
    (should (seq-some (lambda (request)
                        (let ((range (alist-get 'createNamedRange request)))
                          (and range
                               (string-prefix-p
                                "gdocs-org-marker:callout-list:elem-003:bullet:0"
                                (alist-get 'name range)))))
                      requests))
    (should (seq-some (lambda (request)
                        (alist-get 'createParagraphBullets request))
                      requests))
    (should (seq-some (lambda (request)
                        (let* ((update (alist-get 'updateParagraphStyle request))
                               (style (alist-get 'paragraphStyle update)))
                          (and update
                               (alist-get 'indentStart style)
                               (alist-get 'indentFirstLine style))))
                      requests))))

(ert-deftest gdocs-convert-test-callout-block-requests-mark-label-and-body ()
  "Callout requests include generated label and body markers."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "#+ATTR_CALLOUT: :type warning :title \"Heads up\"\n#+BEGIN_CALLOUT\nBody.\n#+END_CALLOUT"))
         (requests (gdocs-convert-ir-to-docs-requests ir)))
    (should (member '((insertText . ((text . "Warning — Heads up\n")
                                     (location . ((index . 1))))))
                    requests))
    (should (seq-some (lambda (request)
                        (let ((range (alist-get 'createNamedRange request)))
                          (and range
                               (string-match-p
                                "gdocs-org-marker:callout-label:elem-[0-9][0-9]*:warning:Heads%20up"
                                (alist-get 'name range)))))
                      requests))
    (should (seq-some (lambda (request)
                        (let ((range (alist-get 'createNamedRange request)))
                          (and range
                               (string-match-p
                                "gdocs-org-marker:callout-body:elem-[0-9][0-9]*:warning:Heads%20up"
                                (alist-get 'name range)))))
                      requests))
    (should (member '((insertText . ((text . "\n")
                                     (location . ((index . 26))))))
                    requests))))

(ert-deftest gdocs-convert-test-docs-json-marked-callout-block-consumes-separator ()
  "Marked callouts consume the generated empty separator paragraph."
  (let* ((body-elements
          (vector `((startIndex . 1)
                    (endIndex . 9)
                    (paragraph
                     . ((elements . ,(vector '((textRun . ((content . "Warning\n")
                                                            (textStyle . ()))))))
                        (paragraphStyle . ((namedStyleType . "NORMAL_TEXT"))))))
                  `((startIndex . 9)
                    (endIndex . 15)
                    (paragraph
                     . ((elements . ,(vector '((textRun . ((content . "Body.\n")
                                                            (textStyle . ()))))))
                        (paragraphStyle . ((namedStyleType . "NORMAL_TEXT"))))))
                  `((startIndex . 15)
                    (endIndex . 16)
                    (paragraph
                     . ((elements . ,(vector '((textRun . ((content . "\n")
                                                            (textStyle . ()))))))
                        (paragraphStyle . ((namedStyleType . "NORMAL_TEXT"))))))))
         (json `((body . ((content . ,body-elements)))
                 (namedRanges
                  . ,(list
                      (cons (intern "gdocs-org-marker:callout-label:elem-001:warning:")
                            `((namedRanges . ,(vector `((ranges . ,(vector '((startIndex . 1) (endIndex . 8)))))))))
                      (cons (intern "gdocs-org-marker:callout-body:elem-001:warning:")
                            `((namedRanges . ,(vector `((ranges . ,(vector '((startIndex . 9) (endIndex . 14)))))))))))))
         (ir (gdocs-convert-docs-json-to-ir json)))
    (should (= (length ir) 1))
    (should (eq (plist-get (car ir) :type) 'callout-block))
    (should (= (plist-get (car ir) :doc-end) 16))))

(ert-deftest gdocs-convert-test-docs-json-marked-callout-list-block ()
  "Marked callout list body paragraphs pull back inside Org callouts."
  (let* ((body-elements
          (vector `((startIndex . 1)
                    (endIndex . 5)
                    (paragraph
                     . ((elements . ,(vector '((textRun . ((content . "Tip\n")
                                                            (textStyle . ()))))))
                        (paragraphStyle . ((namedStyleType . "NORMAL_TEXT"))))))
                  `((startIndex . 5)
                    (endIndex . 9)
                    (paragraph
                     . ((elements . ,(vector '((textRun . ((content . "- one\n")
                                                            (textStyle . ()))))))
                        (paragraphStyle . ((namedStyleType . "NORMAL_TEXT"))))))
                  `((startIndex . 9)
                    (endIndex . 13)
                    (paragraph
                     . ((elements . ,(vector '((textRun . ((content . "- two\n")
                                                            (textStyle . ()))))))
                        (paragraphStyle . ((namedStyleType . "NORMAL_TEXT"))))))))
         (json `((body . ((content . ,body-elements)))
                 (namedRanges
                  . ,(list
                      (cons (intern "gdocs-org-marker:callout-label:elem-001:tip:")
                            `((namedRanges . ,(vector `((ranges . ,(vector '((startIndex . 1) (endIndex . 4)))))))))
                      (cons (intern "gdocs-org-marker:callout-body:elem-001:tip:")
                            `((namedRanges . ,(vector `((ranges . ,(vector '((startIndex . 5) (endIndex . 12)))))))))
                      (cons (intern "gdocs-org-marker:callout-list:elem-001:bullet:0")
                            `((namedRanges . ,(vector `((ranges . ,(vector '((startIndex . 5) (endIndex . 12)))))))))))))
         (org (gdocs-convert-ir-to-org (gdocs-convert-docs-json-to-ir json))))
    (should (string-search "#+BEGIN_CALLOUT\n- one\n- two\n#+END_CALLOUT" org))))

(ert-deftest gdocs-convert-test-docs-json-marked-callout-block ()
  "Marked callout label/body paragraphs pull back to Org callouts."
  (let* ((body-elements
          (vector `((startIndex . 1)
                    (endIndex . 20)
                    (paragraph
                     . ((elements . ,(vector '((textRun . ((content . "Warning — Heads up\n")
                                                            (textStyle . ()))))))
                        (paragraphStyle . ((namedStyleType . "NORMAL_TEXT"))))))
                  `((startIndex . 20)
                    (endIndex . 26)
                    (paragraph
                     . ((elements . ,(vector '((textRun . ((content . "Body.\n")
                                                            (textStyle . ()))))))
                        (paragraphStyle . ((namedStyleType . "NORMAL_TEXT"))))))))
         (json `((body . ((content . ,body-elements)))
                 (namedRanges
                  . ,(list
                      (cons (intern "gdocs-org-marker:callout-label:elem-001:warning:Heads%20up")
                            `((namedRanges . ,(vector `((ranges . ,(vector '((startIndex . 1) (endIndex . 19)))))))))
                      (cons (intern "gdocs-org-marker:callout-body:elem-001:warning:Heads%20up")
                            `((namedRanges . ,(vector `((ranges . ,(vector '((startIndex . 20) (endIndex . 25)))))))))))))
         (ir (gdocs-convert-docs-json-to-ir json))
         (org (gdocs-convert-ir-to-org ir)))
    (should (= (length ir) 1))
    (should (eq (plist-get (car ir) :type) 'callout-block))
    (should (string= (s-trim org)
                     "#+ATTR_CALLOUT: :type warning :title \"Heads up\"\n#+BEGIN_CALLOUT\nBody.\n#+END_CALLOUT"))))

;; ---------------------------------------------------------------------------
;;; Src block conversion

(ert-deftest gdocs-convert-test-quote-block-requests-use-line-role-styles ()
  "Quote block requests use semantic markers and line-role styles."
  (let* ((gdocs-style-definitions
          '((gdocs-quote-block-first
             :parent normal
             :paragraph (:space-above 6
                         :indent-start 36
                         :indent-first-line 0
                         :background-color "#f5f5f5"))
            (gdocs-quote-block-last :parent normal :paragraph (:space-below 6))))
         (ir (gdocs-convert-org-string-to-ir
              "#+BEGIN_QUOTE\nLine one\n\nLine two\n#+END_QUOTE"))
         (requests (gdocs-convert-ir-to-docs-requests ir)))
    (should (member '((createNamedRange
                       . ((name . "gdocs-org-marker:quote-block:elem-001")
                          (range . ((startIndex . 1)
                                    (endIndex . 9))))))
                    requests))
    (should (seq-some (lambda (request)
                        (let ((update (alist-get 'updateParagraphStyle request)))
                          (and update
                               (equal (alist-get 'fields update)
                                      "namedStyleType,shading.backgroundColor,indentFirstLine,indentStart,spaceAbove"))))
                      requests))
    (should (seq-some (lambda (request)
                        (let ((update (alist-get 'updateParagraphStyle request)))
                          (and update
                               (equal (alist-get 'fields update)
                                      "namedStyleType,spaceBelow"))))
                      requests))))

(ert-deftest gdocs-convert-test-src-block-to-ir ()
  "A src block produces a semantic source-block IR element."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "#+begin_src emacs-lisp\n(message \"hello\")\n#+end_src"))
         (elem (car ir)))
    (should (= (length ir) 1))
    (should (eq (plist-get elem :type) 'source-block))
    (should (equal (plist-get elem :language) "emacs-lisp"))
    (should (equal (plist-get elem :lines) '("(message \"hello\")")))))

(ert-deftest gdocs-convert-test-block-line-style-name-is-generic ()
  "Typographic block line roles use a reusable naming convention."
  (should (eq (gdocs-convert--block-line-style-name 'gdocs-source-block 0 1)
              'gdocs-source-block-single))
  (should (eq (gdocs-convert--block-line-style-name 'gdocs-quote-block 0 3)
              'gdocs-quote-block-first))
  (should (eq (gdocs-convert--block-line-style-name 'gdocs-quote-block 1 3)
              'gdocs-quote-block-line))
  (should (eq (gdocs-convert--block-line-style-name 'gdocs-quote-block 2 3)
              'gdocs-quote-block-last)))

(ert-deftest gdocs-convert-test-src-block-preserves-blank-lines ()
  "Source block blank lines stay inside a semantic block element."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "#+begin_src emacs-lisp\n(message \"hello\")\n\n(+ 1 2)\n#+end_src"))
         (block (car ir))
         (org (gdocs-convert-ir-to-org ir)))
    (should (= (length ir) 1))
    (should (eq (plist-get block :type) 'source-block))
    (should (equal (plist-get block :lines)
                   '("(message \"hello\")" "" "(+ 1 2)")))
    (should (string= (s-trim org)
                     "#+BEGIN_SRC emacs-lisp\n(message \"hello\")\n\n(+ 1 2)\n#+END_SRC"))))

(ert-deftest gdocs-convert-test-code-style-policy-is-overridable ()
  "Logical code style can be changed without changing code semantics."
  (let* ((gdocs-style-definitions
          '((gdocs-code :text (:font-family "Roboto Mono"))))
         (ir (gdocs-convert-org-string-to-ir "~code~"))
         (requests (gdocs-convert-ir-to-docs-requests ir)))
    (should (member '((updateTextStyle
                       . ((textStyle
                           . ((weightedFontFamily
                               . ((fontFamily . "Roboto Mono")))))
                          (range . ((startIndex . 1)
                                    (endIndex . 5)))
                          (fields . "weightedFontFamily"))))
                    requests))))

(ert-deftest gdocs-convert-test-src-block-empty-line-marker-has-nonempty-range ()
  "Source block empty-line markers cover the newline, not a zero-length range."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "#+begin_src emacs-lisp\n(message \"hello\")\n\n(+ 1 2)\n#+end_src"))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (marker-requests
          (cl-remove-if-not (lambda (request)
                              (alist-get 'createNamedRange request))
                            requests)))
    (dolist (request marker-requests)
      (let* ((range (alist-get 'range
                               (alist-get 'createNamedRange request)))
             (start (alist-get 'startIndex range))
             (end (alist-get 'endIndex range)))
        (should (> end start))))))

(ert-deftest gdocs-convert-test-src-block-requests-mark-language ()
  "Source block requests create a marker named range with the language."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "#+begin_src emacs-lisp\n(message \"hello\")\n#+end_src"))
         (requests (gdocs-convert-ir-to-docs-requests ir)))
    (should (member '((createNamedRange
                       . ((name . "gdocs-org-marker:src-block:elem-001:emacs-lisp")
                          (range . ((startIndex . 1)
                                    (endIndex . 18))))))
                    requests))))

;; ---------------------------------------------------------------------------
;;; Round-trip stability for complex documents

(ert-deftest gdocs-convert-test-round-trip-horizontal-rule ()
  "A horizontal rule survives org -> IR -> org round-trip."
  (let* ((original "First paragraph\n\n-----\n\nSecond paragraph\n")
         (ir (gdocs-convert-org-string-to-ir original))
         (result (gdocs-convert-ir-to-org ir))
         ;; Re-parse result to verify semantic equivalence
         (re-ir (gdocs-convert-org-string-to-ir result)))
    ;; The round-trip should preserve all three elements
    (should (= (length re-ir) 3))
    (should (gdocs-convert-test--find-element re-ir 'horizontal-rule))
    (should (string-match-p "-----" result))
    (should (string-match-p "First paragraph" result))
    (should (string-match-p "Second paragraph" result))))

(ert-deftest gdocs-convert-test-round-trip-multiple-headings ()
  "Multiple headings at different levels survive round-trip."
  (let* ((original "* Heading one\n\n** Heading two\n\n*** Heading three\n")
         (ir (gdocs-convert-org-string-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    (should (string= (s-trim result) (s-trim original)))))

(ert-deftest gdocs-convert-test-round-trip-mixed-formatting-paragraph ()
  "Paragraph with bold, italic, AND code in one line survives round-trip."
  (let* ((original "Here is *bold*, /italic/, and ~code~ together.\n")
         (ir (gdocs-convert-org-string-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    (should (string= (s-trim result) (s-trim original)))))

;; ---------------------------------------------------------------------------
;;; Segmented parsing edge cases

(ert-deftest gdocs-convert-test-segments-preamble-captured ()
  "Preamble text before the first body element is captured."
  (with-temp-buffer
    (insert "#+STARTUP: showall\n\n* Heading\n\nBody text\n")
    (org-mode)
    (let* ((result (gdocs-convert-org-buffer-to-segments))
           (preamble (plist-get result :preamble)))
      ;; The preamble should contain the STARTUP keyword
      (should (stringp preamble))
      (should (string-match-p "STARTUP" preamble)))))

(ert-deftest gdocs-convert-test-segments-no-postamble ()
  "No postamble with property-drawer storage."
  (with-temp-buffer
    (insert ":PROPERTIES:\n:GDOCS_DOCUMENT_ID: abc\n:END:\n* Heading\n\nBody text\n")
    (org-mode)
    (let* ((result (gdocs-convert-org-buffer-to-segments))
           (postamble (plist-get result :postamble)))
      (should (stringp postamble))
      (should (string= postamble "")))))

(ert-deftest gdocs-convert-test-segments-count-matches-ir ()
  "Number of segments matches number of body IR elements."
  (with-temp-buffer
    (insert "#+TITLE: Doc\n\n* Heading one\n\nParagraph one\n\n* Heading two\n\nParagraph two\n")
    (org-mode)
    (let* ((result (gdocs-convert-org-buffer-to-segments))
           (ir (plist-get result :ir))
           (segments (plist-get result :segments))
           ;; Body elements = all IR minus the title element
           (title-count (if (eq (plist-get (car ir) :style) 'title) 1 0))
           (body-count (- (length ir) title-count)))
      (should (= (length segments) body-count)))))

;; ---------------------------------------------------------------------------
;;; IR to org edge cases

(ert-deftest gdocs-convert-test-ir-to-org-empty-ir ()
  "An empty IR produces a minimal string."
  (let ((result (gdocs-convert-ir-to-org nil)))
    (should (stringp result))
    (should (string= (s-trim result) ""))))

(ert-deftest gdocs-convert-test-ir-to-org-empty-paragraph ()
  "An IR with a paragraph that has nil contents is skipped."
  (let* ((ir (list (list :type 'paragraph
                         :style 'normal
                         :contents nil
                         :id "elem-001")))
         (result (gdocs-convert-ir-to-org ir)))
    (should (stringp result))
    ;; Empty paragraph is skipped, result should be empty/whitespace
    (should (string= (s-trim result) ""))))

;; ---------------------------------------------------------------------------
;;; Org-to-IR formatting edge cases

(ert-deftest gdocs-convert-test-nested-bold-inside-link ()
  "Bold text inside a link produces a run with both :bold and :link."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "Click [[https://example.com][*here*]]"))
         (runs (plist-get (car ir) :contents))
         ;; Find the run that has the link
         (link-run (--first (plist-get it :link) runs)))
    (should link-run)
    (should (string= (plist-get link-run :link) "https://example.com"))
    (should (plist-get link-run :bold))
    (should (string= (plist-get link-run :text) "here"))))

(ert-deftest gdocs-convert-test-adjacent-formatting-runs ()
  "Adjacent *bold* /italic/ produces separate bold and italic runs."
  (let* ((ir (gdocs-convert-org-string-to-ir "*bold* /italic/"))
         (runs (plist-get (car ir) :contents)))
    ;; Should have at least a bold run and an italic run
    (should (>= (length runs) 2))
    (let ((bold-run (--first (plist-get it :bold) runs))
          (italic-run (--first (plist-get it :italic) runs)))
      (should bold-run)
      (should (string= (plist-get bold-run :text) "bold"))
      (should italic-run)
      (should (string= (plist-get italic-run :text) "italic")))))

;; ---------------------------------------------------------------------------
;;; Docs JSON with multiple paragraphs

(ert-deftest gdocs-convert-test-docs-json-multiple-paragraphs ()
  "JSON with 2+ paragraphs produces the correct IR count."
  (let* ((json `((title . "Test Doc")
                 (body
                  . ((content
                      . [((paragraph
                           . ((elements
                               . [((textRun
                                    . ((content . "First paragraph\n")
                                       (textStyle . ()))))])
                              (paragraphStyle
                               . ((namedStyleType . "NORMAL_TEXT"))))))
                         ((paragraph
                           . ((elements
                               . [((textRun
                                    . ((content . "Second paragraph\n")
                                       (textStyle . ()))))])
                              (paragraphStyle
                               . ((namedStyleType . "NORMAL_TEXT"))))))
                         ((paragraph
                           . ((elements
                               . [((textRun
                                    . ((content . "Third paragraph\n")
                                       (textStyle . ()))))])
                              (paragraphStyle
                               . ((namedStyleType . "NORMAL_TEXT"))))))])))))
         (ir (gdocs-convert-docs-json-to-ir json)))
    ;; 1 title + 3 body paragraphs = 4 elements
    (should (= (length ir) 4))
    ;; First element is the title
    (should (eq (plist-get (nth 0 ir) :style) 'title))
    ;; Body paragraphs
    (should (string= (plist-get (car (plist-get (nth 1 ir) :contents)) :text)
                      "First paragraph"))
    (should (string= (plist-get (car (plist-get (nth 2 ir) :contents)) :text)
                      "Second paragraph"))
    (should (string= (plist-get (car (plist-get (nth 3 ir) :contents)) :text)
                      "Third paragraph"))))

(ert-deftest gdocs-convert-test-empty-run-no-style-request ()
  "Empty-text formatted runs must not produce updateTextStyle requests.
Regression test: the Google API rejects ranges where startIndex == endIndex."
  (let* ((runs (list (list :text "" :bold t)
                     (list :text "hello" :bold t)))
         (requests (gdocs-convert--make-text-style-requests runs 1)))
    ;; Only the non-empty run should produce a request.
    (should (= (length requests) 1))
    (let* ((req (car requests))
           (range (alist-get 'range (alist-get 'updateTextStyle req))))
      (should (< (alist-get 'startIndex range)
                 (alist-get 'endIndex range))))))

(provide 'gdocs-convert-test)
;;; gdocs-convert-test.el ends here
