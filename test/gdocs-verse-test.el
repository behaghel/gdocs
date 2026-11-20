;;; gdocs-verse-test.el --- Verse fidelity and feature boundaries -*- lexical-binding: t; -*-

;;; Commentary:
;; Hard lines, stanza gaps, indentation and ordinary rich text are authored data.

;;; Code:

(require 'ert)
(require 'json)
(require 'gdocs-fidelity-corrections-test)

(defconst gdocs-verse-test--directory (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the native verse captures.")

(ert-deftest gdocs-verse-create-pull-and-repeat-preserves-hard-lines ()
  "A verse survives the full local publication flow without becoming prose."
  (let* ((source "#+BEGIN_VERSE\nLine one\nLine two\n#+END_VERSE\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (native (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json requests))))
    (should (eq (plist-get (car ir) :type) 'verse-block))
    (should (equal (gdocs-convert-ir-to-org ir) source))
    (should (equal (gdocs-convert-ir-to-org native) source))
    (should-not (gdocs-diff-generate native ir))
    (should-not (gdocs-diff-generate native (gdocs-convert-org-string-to-ir (gdocs-convert-ir-to-org native))))))

(ert-deftest gdocs-verse-rich-stanzas-retain-indentation-and-spaces ()
  "Rich runs retain authored lines, blank stanzas, spaces, links and Unicode."
  (let* ((source "#+BEGIN_VERSE\n  First *bold*.\nSecond /italic/ 😀.\n\n    Third [[https://example.org][link]].  \n#+END_VERSE\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (before (copy-tree ir))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (native (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json requests))))
    (should (equal (gdocs-convert-ir-to-org ir) source))
    (should (equal (gdocs-convert-ir-to-org native) source))
    (should (equal ir before))
    (should-not (gdocs-diff-generate native ir))))

(ert-deftest gdocs-verse-edits-preserve-following-body-and-repeat ()
  "Edits use exact UTF-16 block geometry and leave the following paragraph intact."
  (let* ((source "#+BEGIN_VERSE\nLine one\nLine two\n#+END_VERSE\n\nAfter 😀.\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (initial (gdocs-convert-ir-to-docs-requests ir))
         (native (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json initial)))
         (updated (string-replace "Line two" "  Second *😀*." source))
         (new (gdocs-convert-org-string-to-ir updated))
         (requests (gdocs-diff-generate native new))
         (result (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json (append initial requests)))))
    (should requests)
    (should (equal (gdocs-convert-ir-to-org result) updated))
    (should-not (gdocs-diff-generate result new))))

(ert-deftest gdocs-verse-adjacent-blocks-and-edge-blank-lines ()
  "Distinct verses retain block boundaries and significant edge blank lines."
  (dolist (source '("Before.\n\n#+BEGIN_VERSE\none\n#+END_VERSE\n\n#+BEGIN_VERSE\n\n  two\n\n#+END_VERSE\n"
                    "#+BEGIN_VERSE\n#+END_VERSE\n"
                    "#+BEGIN_VERSE\n\n#+END_VERSE\n"))
    (let* ((ir (gdocs-convert-org-string-to-ir source))
           (native (gdocs-convert-docs-json-to-ir
                    (gdocs-fidelity-test--returned-json (gdocs-convert-ir-to-docs-requests ir)))))
      (should (equal (gdocs-convert-ir-to-org ir) source))
      (should (equal (gdocs-convert-ir-to-org native) source))
      (should-not (gdocs-diff-generate native ir)))))

(ert-deftest gdocs-verse-unsupported-source-fails-before-publication ()
  "Recognized unsupported semantic content is not flattened into verse text."
  (dolist (body '("<2026-11-18 Wed>" "text[fn:one]" "src_python{x + 1}"))
    (should-error (gdocs-convert-org-string-to-ir (format "#+BEGIN_VERSE\n%s\n#+END_VERSE\n" body)) :type 'user-error))
  (dolist (wrapper '("QUOTE" "CENTER"))
    (should-error (gdocs-convert-org-string-to-ir
                   (format "#+BEGIN_%s\n#+BEGIN_VERSE\nline\n#+END_VERSE\n#+END_%s\n" wrapper wrapper))
                  :type 'user-error)))

(ert-deftest gdocs-verse-native-boundaries-and-legacy-metadata-fail-closed ()
  "Missing version or partial native line coverage cannot silently become prose."
  (let* ((ir (gdocs-convert-org-string-to-ir "#+BEGIN_VERSE\none\n\ntwo\n#+END_VERSE\n"))
         (initial (gdocs-fidelity-test--returned-json (gdocs-convert-ir-to-docs-requests ir))))
    (dolist (kind '(legacy partial object page-break empty-range))
      (let* ((document (copy-tree initial t))
             (entry (car (alist-get 'namedRanges document))))
        (pcase kind
          ('legacy (setcar entry (intern (string-remove-suffix ":v1" (symbol-name (car entry))))))
          ('partial
           (let* ((named (elt (alist-get 'namedRanges (cdr entry)) 0))
                  (range (elt (alist-get 'ranges named) 0)))
             (setf (alist-get 'startIndex range) 2)))
          ('empty-range
           (setf (alist-get 'ranges (elt (alist-get 'namedRanges (cdr entry)) 0)) []))
          ('page-break
           (let* ((paragraph (seq-find (lambda (element) (alist-get 'paragraph element))
                                       (alist-get 'content (alist-get 'body document))))
                  (part (elt (alist-get 'elements (alist-get 'paragraph paragraph)) 0)))
             (setf (alist-get 'textRun part) nil
                   (alist-get 'pageBreak part) '((textStyle . nil)))))
          ('object
           (let* ((paragraph (seq-find (lambda (element) (alist-get 'paragraph element))
                                       (alist-get 'content (alist-get 'body document))))
                  (part (elt (alist-get 'elements (alist-get 'paragraph paragraph)) 0)))
             (setf (alist-get 'textRun part) nil
                   (alist-get 'footnoteReference part) '((footnoteId . "unsupported"))))))
        (should-error (gdocs-convert-docs-json-to-ir document) :type 'user-error)))))

(ert-deftest gdocs-verse-native-line-edit-retains-literal-layout ()
  "Native text edits keep the same verse and cannot trigger immediate rewrite."
  (let* ((source "#+BEGIN_VERSE\n  one 😀\n\ntwo\n#+END_VERSE\n")
         (document (gdocs-fidelity-test--returned-json
                    (gdocs-convert-ir-to-docs-requests (gdocs-convert-org-string-to-ir source)))))
    (seq-doseq (element (alist-get 'content (alist-get 'body document)))
      (seq-doseq (part (alist-get 'elements (alist-get 'paragraph element)))
        (when-let* ((run (alist-get 'textRun part)))
          (setf (alist-get 'content run) (string-replace "two" "new" (alist-get 'content run))))))
    (let* ((native (gdocs-convert-docs-json-to-ir document))
           (expected (string-replace "two" "new" source)))
      (should (equal (gdocs-convert-ir-to-org native) expected))
      (should-not (gdocs-diff-generate native (gdocs-convert-org-string-to-ir expected))))))

(ert-deftest gdocs-verse-multiple-block-edits-preserve-rich-lines-and-following-text ()
  "Fixture-level create/edit/repeat covers multiple blocks and shifted geometry."
  (let* ((source (with-temp-buffer
                   (insert-file-contents (expand-file-name "test/fixtures/disposable-verse-conformance.org"
                                                           (file-name-directory (locate-library "gdocs-convert-verse"))))
                   (buffer-string)))
         (initial (gdocs-convert-ir-to-docs-requests (gdocs-convert-org-string-to-ir source)))
         (native (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json initial)))
         (updated (string-replace "second /emphasized/" "  second revised\nthird /emphasized/"
                                  (string-replace "Separate poem." "*Separate* poem."
                                                  (string-replace "After both verses 😀." "After both verses, edited 😀." source))))
         (new (gdocs-convert-org-string-to-ir updated))
         (edits (gdocs-diff-generate native new))
         (result (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json (append initial edits)))))
    (should edits)
    (should (equal (gdocs-convert-ir-to-org result) updated))
    (should-not (gdocs-diff-generate result new))))

(ert-deftest gdocs-verse-native-inserted-line-is-authoritative ()
  "Native insertion extends the structural range without a hidden line-count snapshot."
  (let* ((source "#+BEGIN_VERSE\none 😀 tail\n#+END_VERSE\n")
         (initial (gdocs-convert-ir-to-docs-requests (gdocs-convert-org-string-to-ir source)))
         (document (gdocs-fidelity-test--returned-json
                    (append initial '(((insertText . ((text . "\n  native") (location . ((index . 4))))))))))
         (native (gdocs-convert-docs-json-to-ir document))
         (expected "#+BEGIN_VERSE\none\n  native 😀 tail\n#+END_VERSE\n"))
    (should (equal (gdocs-convert-ir-to-org native) expected))
    (should-not (gdocs-diff-generate native (gdocs-convert-org-string-to-ir expected)))))

(ert-deftest gdocs-verse-feature-owns-policy ()
  "Recognition, rendering, grouping and diff policy have feature owners."
  (dolist (function '(gdocs-convert--verse-block-to-ir gdocs-convert--verse-block-to-requests
                                                       gdocs-convert--ir-verse-block-to-org gdocs-convert--attach-verse-block-markers
                                                       gdocs-convert-verse-validate-native-marker gdocs-convert-verse-validate-native-context))
    (should (string-match-p "gdocs-convert-verse\\.elc?\\'" (symbol-file function 'defun))))
  (should (string-match-p "gdocs-diff-verse\\.elc?\\'" (symbol-file 'gdocs-diff-verse--element 'defun))))

(ert-deftest gdocs-verse-generic-engines-remain-feature-neutral ()
  "Diff algorithms and shared codecs do not recognize verse as a semantic type."
  (cl-labels ((recognizes (tree)
                (and (consp tree)
                     (or (equal tree ''verse-block) (recognizes (car tree)) (recognizes (cdr tree))))))
             (dolist (file '("gdocs-diff.el" "gdocs-convert-text.el" "gdocs-convert-inline-codec.el" "gdocs-convert-paragraph-codec.el"))
               (with-temp-buffer
                 (insert-file-contents (expand-file-name file (file-name-directory (locate-library "gdocs-convert"))))
                 (goto-char (point-min))
                 (condition-case nil
                     (while t (should-not (recognizes (read (current-buffer)))))
                   (end-of-file nil))))))

(ert-deftest gdocs-verse-standalone-line-assembly-is-neutral ()
  "Verse line assembly loads without facade, presentation, transport or accounts."
  (with-temp-buffer
    (let* ((program '(progn
                       (require 'gdocs-convert-verse)
                       (dolist (feature '(gdocs-convert gdocs-styles gdocs-style-resolve gdocs-sync gdocs-api gdocs-auth))
                         (when (featurep feature) (error "Verse assembly loaded %s" feature)))
                       (unless (equal (gdocs-convert-verse--split-runs '((:text "  one\n\n two  " :bold t)))
                                      '(((:text "  one" :bold t)) nil ((:text " two  " :bold t))))
                         (error "Standalone verse assembly changed literal rich lines"))))
           (status (call-process (expand-file-name invocation-name invocation-directory) nil t nil
                                 "--batch" "-Q" "-L" (file-name-directory (locate-library "gdocs-convert"))
                                 "--eval" (prin1-to-string program))))
      (ert-info ((buffer-string)) (should (zerop status))))))

(ert-deftest gdocs-verse-live-captures-preserve-rich-lines-and-native-insertion ()
  "Actual create/edit/native-insert responses retain independent verse ownership."
  (let ((created "#+BEGIN_VERSE\n  First *bold* line 😀.\nsecond /emphasized/ line with [[https://example.org][a link]].\n\n    A stanza with ~literal  spaces~.\n  final line.  \n#+END_VERSE\n")
        (edited "#+BEGIN_VERSE\n    First *bold revised* line 😀.\nsecond emphasized line with [[https://example.org][a link]].\n\n    A stanza with ~literal  spaces~.\n  added /line/ 😀.\n  final line.  \n#+END_VERSE\n"))
    (dolist (stage '("created" "edited" "native-inserted"))
      (let* ((json-object-type 'alist) (json-array-type 'vector)
             (document (json-read-file (expand-file-name (format "fixtures/live-native-verse-%s.json" stage) gdocs-verse-test--directory)))
             (native (seq-filter #'gdocs-convert-verse-element-p (gdocs-convert-docs-json-to-ir document)))
             (first (pcase stage ("created" created) ("edited" edited)
                           (_ (string-replace "second emphasized" "second\n  native added line 😀 emphasized" edited))))
             (second (if (equal stage "created") "#+BEGIN_VERSE\nSeparate poem.\n#+END_VERSE\n"
                       "#+BEGIN_VERSE\n*Separate* poem.\n#+END_VERSE\n")))
        (should-not (alist-get 'revisionId document))
        (should (= (length native) 2))
        (should (equal (gdocs-convert-ir-to-org (list (car native))) first))
        (should (equal (gdocs-convert-ir-to-org (list (cadr native))) second))
        (should-not (gdocs-diff-generate native (gdocs-convert-org-string-to-ir (concat first "\n" second))))))))

(provide 'gdocs-verse-test)
;;; gdocs-verse-test.el ends here
