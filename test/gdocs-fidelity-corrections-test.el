;;; gdocs-fidelity-corrections-test.el --- Content-loss regression audit -*- lexical-binding: t; -*-

;;; Commentary:
;; Reproduce losses that must not be grandfathered as unchanged behavior.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gdocs-convert)
(require 'gdocs-diff)

(ert-deftest gdocs-fidelity-quote-import-preserves-authored-italic ()
  "Import must not erase italic text when no semantic provenance distinguishes it."
  (let* ((run '(:text "authored" :italic t :bold t :link "https://example.org"))
         (before (copy-tree run))
         (result (car (gdocs-convert--strip-quote-visual-style (list run)))))
    (should (plist-get result :italic))
    (should (plist-get result :bold))
    (should (equal (plist-get result :link) "https://example.org"))
    (should (equal run before))))

(ert-deftest gdocs-fidelity-table-format-only-edits-produce-requests ()
  "A table cell emphasis edit must not disappear as an unchanged table."
  (let* ((old '(:type table :rows ((((:text "cell" :bold nil))))))
         (new (copy-tree old)))
    (plist-put (car (car (car (plist-get new :rows)))) :bold t)
    (should (gdocs-diff-generate (list old) (list new)))))

(ert-deftest gdocs-fidelity-table-row-boundaries-affect-identity ()
  "Different table shapes with identical ordered cell text are not equivalent."
  (let ((one '(:type table :rows ((((:text "a")) ((:text "b"))))))
        (two '(:type table :rows ((((:text "a"))) (((:text "b")))))))
    (should-not (equal (gdocs-diff--element-key one) (gdocs-diff--element-key two)))))

(ert-deftest gdocs-fidelity-distinct-source-blocks-do-not-merge ()
  "Separate native source block IDs retain boundaries even in the same language."
  (let* ((paragraph '(:type paragraph :style normal :contents ((:text "line"))
                            :gdocs-marker ((:type src-block :id "one" :data "python"))))
         (other (copy-tree paragraph)))
    (plist-put (car (plist-get other :gdocs-marker)) :id "two")
    (should (= (length (gdocs-convert--attach-source-block-markers (list paragraph other))) 2))))

(ert-deftest gdocs-fidelity-inline-source-retains-language ()
  "Inline source language and code survive the source round trip."
  (let* ((source "Result: src_python{1+1}\n")
         (ir (gdocs-convert-org-string-to-ir source)))
    (should (equal (gdocs-convert-ir-to-org ir) source))))

(ert-deftest gdocs-fidelity-quote-italic-xor-is-reversible ()
  "Authored italic is independent of quote presentation and current configuration."
  (dolist (base '(nil t))
    (let* ((gdocs-style-definitions
            (list (list 'gdocs-quote-block-single :parent 'normal :text (list :italic (if base t :json-false)))))
           (block '(:type quote-block :id "quote" :paragraphs (((:text "plain " :italic nil) (:text "emph😀" :italic t)))))
           (plan (gdocs-convert--quote-block-to-requests block 1))
           (italic-requests (seq-filter
                             (lambda (request)
                               (equal (alist-get 'fields (alist-get 'updateTextStyle request)) "italic"))
                             (plist-get plan :requests)))
           (final (last italic-requests 2))
           (markers (delq nil (mapcar (lambda (request) (alist-get 'createNamedRange request)) (plist-get plan :requests))))
           (marker (seq-find (lambda (entry) (string-prefix-p "gdocs-org-marker:quote-block:" (alist-get 'name entry))) markers)))
      (should (= (length final) 2))
      (should (eq (alist-get 'italic (alist-get 'textStyle (alist-get 'updateTextStyle (car final))))
                  (if base t :json-false)))
      (should (eq (alist-get 'italic (alist-get 'textStyle (alist-get 'updateTextStyle (cadr final))))
                  (if base :json-false t)))
      (should (string-suffix-p (if base ":italic-v1-1" ":italic-v1-0") (alist-get 'name marker)))
      (let* ((native (list (list :text "plain " :italic base)
                           (list :text "emph😀" :italic (not base))))
             ;; Simulate a theme change before pull: only stored metadata counts.
             (gdocs-style-definitions nil)
             (restored (gdocs-convert--strip-quote-visual-style native
                                                                (list :data (if base "italic-v1-1" "italic-v1-0")))))
        (should-not (plist-get (car restored) :italic))
        (should (plist-get (cadr restored) :italic))))))

(defun gdocs-fidelity-test--returned-json (requests)
  "Simulate literal insertion, text styles and named ranges in REQUESTS.
This fixture interpreter tests local composition, not live API acceptance."
  (let ((text "") styles ranges paragraphs (range-id 0))
    (dolist (request requests)
      (when-let* ((insert (alist-get 'insertText request)))
        (let* ((index (alist-get 'index (alist-get 'location insert)))
               (offset (gdocs-convert--utf16-string-index text (1- index))))
          (setq text (concat (substring text 0 offset) (alist-get 'text insert) (substring text offset)))))
      (when-let* ((style (alist-get 'updateTextStyle request))) (push style styles))
      (when-let* ((range (alist-get 'createNamedRange request)))
        (push (cons (intern (alist-get 'name range))
                    (list (cons 'namedRanges
                                (list (list (cons 'namedRangeId (format "range-%d" (cl-incf range-id)))
                                            (cons 'ranges (list (alist-get 'range range)))))))) ranges)))
    (setq styles (nreverse styles))
    (let ((character 0) (position 1))
      (while (< character (length text))
        (let* ((next (1+ (or (string-match "\n" text character) (1- (length text)))))
               (content (substring text character next))
               (end (+ position (gdocs-convert--string-to-utf16-length content)))
               (cuts (list position end)) elements)
          (dolist (style styles)
            (let* ((range (alist-get 'range style)) (a (alist-get 'startIndex range)) (b (alist-get 'endIndex range)))
              (when (and a b (< a end) (> b position))
                (push (max a position) cuts) (push (min b end) cuts))))
          (setq cuts (sort (delete-dups cuts) #'<))
          (while (cdr cuts)
            (let ((a (car cuts)) (b (cadr cuts)) formatting)
              (dolist (style styles)
                (let ((range (alist-get 'range style)))
                  (when (and (<= (alist-get 'startIndex range) a) (>= (alist-get 'endIndex range) b))
                    (dolist (field (split-string (alist-get 'fields style) ","))
                      ;; Only direct text-style fields are relevant to these fixtures.
                      (unless (string-match-p "\\." field)
                        (let ((key (intern field)))
                          (setf (alist-get key formatting) (alist-get key (alist-get 'textStyle style)))))))))
              (push (list (cons 'startIndex a) (cons 'endIndex b)
                          (cons 'textRun (list (cons 'content
                                                     (substring content (gdocs-convert--utf16-string-index content (- a position))
                                                                (gdocs-convert--utf16-string-index content (- b position))))
                                               (cons 'textStyle formatting)))) elements))
            (setq cuts (cdr cuts)))
          (push (list (cons 'startIndex position) (cons 'endIndex end)
                      (cons 'paragraph (list (cons 'elements (nreverse elements))))) paragraphs)
          (setq character next position end))))
    (list (cons 'body (list (cons 'content (nreverse paragraphs)))) (cons 'namedRanges ranges))))

(ert-deftest gdocs-fidelity-quote-full-pipeline-stored-inversion ()
  "Published style inversion survives returned JSON, theme changes and repeated diff."
  (dolist (base '(nil t))
    (let* ((source "#+BEGIN_QUOTE\nPlain /emph😀/ end.\n#+END_QUOTE\n")
           (ir (gdocs-convert-org-string-to-ir source))
           (gdocs-style-definitions (list (list 'gdocs-quote-block-single :text (list :italic (if base t :json-false)))))
           (requests (gdocs-convert-ir-to-docs-requests ir))
           (json (gdocs-fidelity-test--returned-json requests))
           (gdocs-style-definitions nil)
           (remote (gdocs-convert-docs-json-to-ir json)))
      (should (equal (gdocs-convert-ir-to-org remote) source))
      (should (equal (gdocs-diff--element-keys remote) (gdocs-diff--element-keys ir)))
      (should-not (gdocs-diff-generate remote ir))
      (should-not (gdocs-diff-generate remote ir)))))

(ert-deftest gdocs-fidelity-inline-source-native-roundtrip-and-language-edit ()
  "Inline language/header metadata survives native composition and marker replacement."
  (let* ((source "Before src_python[:exports code]{😀+1} after\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (json (gdocs-fidelity-test--returned-json requests))
         (remote (gdocs-convert-docs-json-to-ir json))
         (edited (gdocs-convert-org-string-to-ir (string-replace "python" "ruby" source)))
         (diff (gdocs-diff-generate remote edited)))
    (should (equal (gdocs-convert-ir-to-org remote) source))
    (should-not (gdocs-diff-generate remote ir))
    (should-not (gdocs-diff-generate remote ir))
    (should (alist-get 'deleteNamedRange (car diff)))
    (should (= (seq-count (lambda (request) (alist-get 'createNamedRange request)) diff) 1))))

(ert-deftest gdocs-fidelity-format-boundaries-affect-identity ()
  "Moving an emphasis boundary without changing text is a semantic edit."
  (let ((one '(:type paragraph :style normal :contents ((:text "a" :bold t) (:text "bc"))))
        (two '(:type paragraph :style normal :contents ((:text "ab" :bold t) (:text "c")))))
    (should (gdocs-diff-generate (list one) (list two)))))

(ert-deftest gdocs-fidelity-legacy-quote-provenance-fails-closed ()
  "Missing style provenance cannot silently erase or invent authored emphasis."
  (should-error (gdocs-convert--strip-quote-visual-style
                 '((:text "ambiguous" :italic t)) '(:type quote-block :id "legacy")) :type 'user-error))

(ert-deftest gdocs-fidelity-quote-paragraph-and-block-boundaries ()
  "Lowercase paragraphs and adjacent quote blocks retain authored boundaries."
  (dolist (source '("#+BEGIN_QUOTE\nFirst.\n\nsecond paragraph.\n#+END_QUOTE\n"
                    "#+BEGIN_QUOTE\nFirst.\n#+END_QUOTE\n\n#+BEGIN_QUOTE\nsecond quote.\n#+END_QUOTE\n"))
    (let* ((ir (gdocs-convert-org-string-to-ir source))
           (native (gdocs-fidelity-test--returned-json (gdocs-convert-ir-to-docs-requests ir)))
           (remote (gdocs-convert-docs-json-to-ir native)))
      (should (equal (gdocs-convert-ir-to-org remote) source))
      (should-not (gdocs-diff-generate remote ir)))))

(ert-deftest gdocs-fidelity-literal-block-trailing-whitespace ()
  "Code/example blocks preserve trailing spaces and authored empty lines."
  (dolist (source '("#+BEGIN_SRC text\n  literal  \n\n#+END_SRC\n"
                    "#+BEGIN_EXAMPLE\n  literal  \n\n#+END_EXAMPLE\n"))
    (let* ((ir (gdocs-convert-org-string-to-ir source))
           (native (gdocs-fidelity-test--returned-json (gdocs-convert-ir-to-docs-requests ir)))
           (remote (gdocs-convert-docs-json-to-ir native)))
      (should (equal (gdocs-convert-ir-to-org ir) source))
      (should (equal (gdocs-convert-ir-to-org remote) source))
      (should-not (gdocs-diff-generate remote ir)))))

(ert-deftest gdocs-fidelity-semantic-spans-ignore-footnote-sentinels ()
  "Formatting identity uses canonical reference text, not temporary insertion text."
  (should (equal (gdocs-diff--runs-formatting-key '((:text "" :footnote-label "1")))
                 (gdocs-diff--runs-formatting-key '((:text "⟦note⟧" :footnote-label "1"))))))

(ert-deftest gdocs-fidelity-inline-source-invalid-native-ranges-fail-closed ()
  "Unrepresentable inline metadata fails before silent language loss."
  (should-error (gdocs-convert-code-validate-native-marker
                 'inline-src-block [((startIndex . 1) (endIndex . 2)) ((startIndex . 4) (endIndex . 5))]
                 '((namedRangeId . "range"))) :type 'user-error)
  (should-error (gdocs-convert-code-validate-native-marker
                 'inline-src-block [((startIndex . 1) (endIndex . 2) (segmentId . "fn"))]
                 '((namedRangeId . "range"))) :type 'user-error)
  (should-error (gdocs-convert-code-validate-native-table
                 '((startIndex . 1) (endIndex . 20))
                 '((:type inline-src-block :start 4 :end 8))) :type 'user-error)
  (should-error (gdocs-convert-code--marker-source '(:data "not-json")) :type 'user-error)
  (should-error (gdocs-convert-code-run-marker-request
                 '(:text "x" :inline-source (:language "python" :parameters "x")) 1 2 "fn") :type 'user-error))

(ert-deftest gdocs-fidelity-table-format-edit-native-noop ()
  "A formatting-only table edit emits rich requests and no-ops after native fetch."
  (let* ((old (gdocs-convert-org-string-to-ir "| cell |\n"))
         (new (gdocs-convert-org-string-to-ir "| *cell* |\n"))
         (diff (gdocs-diff-generate old new))
         (cell '((content . [((paragraph . ((elements . [((textRun . ((content . "cell\n") (textStyle . ((bold . t))))))]))))])))
         (table (list '(rows . 1) '(columns . 1) (cons 'tableRows (list (list (cons 'tableCells (list cell)))))))
         (json (list (cons 'body (list (cons 'content (list (list '(startIndex . 1) '(endIndex . 10) (cons 'table table))))))))
         (remote (gdocs-convert-docs-json-to-ir json)))
    (should (seq-some (lambda (request)
                        (eq (alist-get 'bold (alist-get 'textStyle (alist-get 'updateTextStyle request))) t)) diff))
    (should-not (gdocs-diff-generate remote new))
    (should-not (gdocs-diff-generate remote new))))

(ert-deftest gdocs-fidelity-source-block-boundaries-full-native-flow ()
  "Adjacent same-language blocks keep their identities through publication and pull."
  (let* ((source "#+BEGIN_SRC python\nfirst\n#+END_SRC\n\n#+BEGIN_SRC python\nsecond\n#+END_SRC\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (json (gdocs-fidelity-test--returned-json (gdocs-convert-ir-to-docs-requests ir)))
         (remote (gdocs-convert-docs-json-to-ir json)))
    (should (= (length remote) 2))
    (should (equal (gdocs-convert-ir-to-org remote) source))
    (should-not (gdocs-diff-generate remote ir))))

(ert-deftest gdocs-fidelity-remote-quote-emphasis-edit-under-inverted-style ()
  "A native collaborator can change emphasis while the applied base remains italic."
  (let* ((source "#+BEGIN_QUOTE\nPlain /emph😀/ end.\n#+END_QUOTE\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (gdocs-style-definitions '((gdocs-quote-block-single :text (:italic t))))
         (json (gdocs-fidelity-test--returned-json (gdocs-convert-ir-to-docs-requests ir)))
         (paragraph (alist-get 'paragraph (car (alist-get 'content (alist-get 'body json))))))
    (dolist (element (alist-get 'elements paragraph))
      (when-let* ((run (alist-get 'textRun element)))
        (when (string-match-p "emph" (alist-get 'content run))
          ;; Upright -> italic in the document removes authored emphasis.
          (setf (alist-get 'italic (alist-get 'textStyle run)) t))))
    (let* ((gdocs-style-definitions nil)
           (remote (gdocs-convert-docs-json-to-ir json))
           (expected (string-replace "/emph😀/" "emph😀" source)))
      (should (equal (gdocs-convert-ir-to-org remote) expected))
      (should-not (gdocs-diff-generate remote (gdocs-convert-org-string-to-ir expected))))))

(ert-deftest gdocs-fidelity-quote-per-paragraph-style-provenance ()
  "Different first/last quote style bases are each decoded from their own snapshot."
  (let* ((source "#+BEGIN_QUOTE\nFirst /emphasis/.\n\nsecond /emphasis/.\n#+END_QUOTE\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (gdocs-style-definitions '((gdocs-quote-block-first :text (:italic t))
                                    (gdocs-quote-block-last :text (:italic :json-false))))
         (json (gdocs-fidelity-test--returned-json (gdocs-convert-ir-to-docs-requests ir)))
         (gdocs-style-definitions nil)
         (remote (gdocs-convert-docs-json-to-ir json)))
    (should (equal (gdocs-convert-ir-to-org remote) source))
    (should-not (gdocs-diff-generate remote ir))))

(ert-deftest gdocs-fidelity-baseline-format-only-edits ()
  "Subscript/superscript changes are authored edits even when literal text is equal."
  (dolist (property '(:subscript :superscript))
    (let* ((old '(:type paragraph :style normal :contents ((:text "2"))))
           (new (copy-tree old)))
      (plist-put (car (plist-get new :contents)) property t)
      (should (gdocs-diff-generate (list old) (list new))))))

(provide 'gdocs-fidelity-corrections-test)
;;; gdocs-fidelity-corrections-test.el ends here
