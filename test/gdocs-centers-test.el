;;; gdocs-centers-test.el --- Centered block fidelity -*- lexical-binding: t; -*-

;;; Commentary:
;; Centered prose owns paragraph groups, alignment and bidirectional identity.

;;; Code:

(require 'ert)
(require 'json)
(require 'gdocs-fidelity-corrections-test)
(require 'gdocs-sync)

(defconst gdocs-centers-test--directory (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing synthetic centered prose fixtures.")

(defconst gdocs-centers-test--source
  "Before.\n\n#+BEGIN_CENTER\nFirst *bold* 😀.\n\nSecond /italic/ with [[https://example.org][link]].\n#+END_CENTER\n\n#+BEGIN_CENTER\nSeparate poem.\n#+END_CENTER\n\nAfter 😀.\n"
  "Synthetic prose with two independently authored centered blocks.")

(defun gdocs-centers-test--native (requests)
  "Decode REQUESTS through the native model using real sync normalization."
  (gdocs-sync--filter-empty-paragraphs
   (gdocs-sync--filter-title
    (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json requests)))))

(ert-deftest gdocs-centers-create-pull-and-repeat-preserves-blocks ()
  "Rich paragraphs retain their enclosing blocks through create/pull/repeat."
  (let* ((source gdocs-centers-test--source)
         (ir (gdocs-convert-org-string-to-ir source))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (native (gdocs-centers-test--native requests)))
    (should (= (seq-count (lambda (element) (eq (plist-get element :type) 'center-block)) ir) 2))
    (should (equal (gdocs-convert-ir-to-org ir) source))
    (should (equal (gdocs-convert-ir-to-org native) source))
    (should-not (gdocs-diff-generate native ir))))

(ert-deftest gdocs-centers-empty-and-adjacent-groups-survive ()
  "An empty center has identity and cannot merge with an adjacent block."
  (dolist (source '("#+BEGIN_CENTER\n#+END_CENTER\n"
                    "#+BEGIN_CENTER\none\n#+END_CENTER\n\n#+BEGIN_CENTER\ntwo\n#+END_CENTER\n"))
    (let* ((ir (gdocs-convert-org-string-to-ir source))
           (native (gdocs-centers-test--native (gdocs-convert-ir-to-docs-requests ir))))
      (should (equal (gdocs-convert-ir-to-org ir) source))
      (should (equal (gdocs-convert-ir-to-org native) source))
      (should-not (gdocs-diff-generate native ir)))))

(ert-deftest gdocs-centers-edits-preserve-richness-and-following-geometry ()
  "Changes in both blocks retain UTF-16 boundaries and the following paragraph."
  (let* ((initial (gdocs-convert-ir-to-docs-requests (gdocs-convert-org-string-to-ir gdocs-centers-test--source)))
         (native (gdocs-centers-test--native initial))
         (updated (string-replace "Separate poem." "*Separate* poem revised 😀."
                                  (string-replace "After 😀." "After, revised 😀."
                                                  (string-replace "First *bold* 😀." "First *strong* 😀.\n\nNew /paragraph/." gdocs-centers-test--source))))
         (new (gdocs-convert-org-string-to-ir updated))
         (edits (gdocs-diff-generate native new))
         (result (gdocs-centers-test--native (append initial edits))))
    (should edits)
    (should (equal (gdocs-convert-ir-to-org result) updated))
    (should-not (gdocs-diff-generate result new))))

(ert-deftest gdocs-centers-native-edits-remain-authoritative ()
  "Inserted paragraphs and text survive pull without losing the original group."
  (let* ((source "#+BEGIN_CENTER\none 😀 tail\n#+END_CENTER\n\nAfter.\n")
         (initial (gdocs-convert-ir-to-docs-requests (gdocs-convert-org-string-to-ir source)))
         (native (gdocs-centers-test--native (append initial '(((insertText . ((text . "\nnew") (location . ((index . 4))))))))))
         (expected "#+BEGIN_CENTER\none\n\nnew 😀 tail\n#+END_CENTER\n\nAfter.\n"))
    (should (equal (gdocs-convert-ir-to-org native) expected))
    (should-not (gdocs-diff-generate native (gdocs-convert-org-string-to-ir expected)))))

(ert-deftest gdocs-centers-unsupported-source-fails-before-mutation ()
  "Nested constructs and unsupported semantic content cannot become plain text."
  (dolist (body '("<2026-11-18 Wed>" "text[fn:one]" "src_python{x + 1}"
                  "[[file:example.png]]" "\\(x + 1\\)" "- list item" "#+BEGIN_VERSE\nline\n#+END_VERSE"))
    (should-error (gdocs-convert-org-string-to-ir (format "#+BEGIN_CENTER\n%s\n#+END_CENTER\n" body)) :type 'user-error)))

(ert-deftest gdocs-centers-source-soft-wraps-remain-prose ()
  "Centered prose normalizes soft source wraps, unlike literal verse lines."
  (should (equal (gdocs-convert-ir-to-org (gdocs-convert-org-string-to-ir "#+BEGIN_CENTER\none\ntwo\n#+END_CENTER\n"))
                 "#+BEGIN_CENTER\none two\n#+END_CENTER\n")))

(ert-deftest gdocs-centers-native-invalid-context-fails-closed ()
  "Partial ranges, uncentered text and non-text content stop before decoding."
  (let ((initial (gdocs-fidelity-test--returned-json
                  (gdocs-convert-ir-to-docs-requests (gdocs-convert-org-string-to-ir "#+BEGIN_CENTER\none\n#+END_CENTER\n")))))
    (dolist (kind '(alignment object partial empty-range version))
      (let* ((document (copy-tree initial t))
             (entry (car (alist-get 'namedRanges document)))
             (range-entry (elt (alist-get 'namedRanges (cdr entry)) 0))
             (paragraph (alist-get 'paragraph (seq-find (lambda (element) (alist-get 'paragraph element))
                                                        (alist-get 'content (alist-get 'body document))))))
        (pcase kind
          ('alignment (setf (alist-get 'alignment (alist-get 'paragraphStyle paragraph)) "START"))
          ('object (let ((part (elt (alist-get 'elements paragraph) 0)))
                     (setf (alist-get 'textRun part) nil (alist-get 'pageBreak part) '((textStyle . nil)))))
          ('partial (setf (alist-get 'startIndex (elt (alist-get 'ranges range-entry) 0)) 2))
          ('empty-range (setf (alist-get 'ranges range-entry) []))
          ('version (setcar entry (intern (string-remove-suffix ":v1" (symbol-name (car entry)))))))
        (should-error (gdocs-convert-docs-json-to-ir document) :type 'user-error)))))

(ert-deftest gdocs-centers-feature-owns-policy ()
  "Center recognition, rendering, validation, identity and geometry have owners."
  (dolist (function '(gdocs-convert--center-block-to-ir gdocs-convert--center-block-to-requests
                                                        gdocs-convert--ir-center-block-to-org gdocs-convert--attach-center-block-markers
                                                        gdocs-convert-center-validate-native-context gdocs-convert-center-paragraph-to-org))
    (should (string-match-p "gdocs-convert-centers\\.elc?\\'" (symbol-file function 'defun))))
  (should (string-match-p "gdocs-diff-centers\\.elc?\\'" (symbol-file 'gdocs-diff-centers--element 'defun))))

(ert-deftest gdocs-centers-generic-engines-remain-neutral ()
  "Generic codecs and diff do not recognize centered semantic groups."
  (cl-labels ((recognizes (tree) (and (consp tree) (or (equal tree ''center-block)
                                                       (recognizes (car tree)) (recognizes (cdr tree))))))
             (dolist (file '("gdocs-diff.el" "gdocs-convert-text.el" "gdocs-convert-inline-codec.el" "gdocs-convert-paragraph-codec.el"))
               (with-temp-buffer
                 (insert-file-contents (expand-file-name file (file-name-directory (locate-library "gdocs-convert"))))
                 (goto-char (point-min))
                 (condition-case nil (while t (should-not (recognizes (read (current-buffer))))) (end-of-file nil))))))

(ert-deftest gdocs-centers-standalone-neutral-loading ()
  "Centered paragraph rendering loads without facade, styles or transport."
  (with-temp-buffer
    (let* ((program '(progn
                       (require 'gdocs-convert-centers)
                       (dolist (feature '(gdocs-convert gdocs-styles gdocs-style-resolve gdocs-sync gdocs-api gdocs-auth))
                         (when (featurep feature) (error "Center feature loaded %s" feature)))
                       (unless (equal (gdocs-convert-center-paragraph-to-org '(:alignment center) "one")
                                      "#+BEGIN_CENTER\none\n#+END_CENTER")
                         (error "Unmarked centered paragraph rendering changed"))))
           (status (call-process (expand-file-name invocation-name invocation-directory) nil t nil
                                 "--batch" "-Q" "-L" (file-name-directory (locate-library "gdocs-convert"))
                                 "--eval" (prin1-to-string program))))
      (ert-info ((buffer-string)) (should (zerop status))))))

(ert-deftest gdocs-centers-standard-fixture-create-edit-and-repeat ()
  "Synthetic fixture covers populated and empty blocks alongside ordinary prose."
  (let* ((source (with-temp-buffer
                   (insert-file-contents (expand-file-name "test/fixtures/disposable-center-conformance.org"
                                                           (file-name-directory (locate-library "gdocs-convert"))))
                   (buffer-string)))
         (initial (gdocs-convert-ir-to-docs-requests (gdocs-convert-org-string-to-ir source)))
         (native (gdocs-centers-test--native initial))
         (updated (string-replace "First *bold* centered paragraph 😀." "First *strong* centered paragraph 😀.\n\nAdded /paragraph/."
                                  (string-replace "Separate centered block." "*Separate* centered block revised."
                                                  (string-replace "After centered prose 😀." "After revised prose 😀." source))))
         (new (gdocs-convert-org-string-to-ir updated))
         (result (gdocs-centers-test--native (append initial (gdocs-diff-generate native new)))))
    (should (equal (gdocs-convert-ir-to-org native) source))
    (should-not (gdocs-diff-generate native (gdocs-convert-org-string-to-ir source)))
    (should (equal (gdocs-convert-ir-to-org result) updated))
    (should-not (gdocs-diff-generate result new))))

(ert-deftest gdocs-centers-live-captures-preserve-groups-and-native-paragraphs ()
  "Actual create/edit/native-insert responses retain rich block identity."
  (let ((created "#+BEGIN_CENTER\nFirst *bold* centered paragraph 😀.\n\nSecond /italic/ paragraph with [[https://example.org][a link]].\n#+END_CENTER\n")
        (edited "#+BEGIN_CENTER\nFirst *strong* centered paragraph 😀.\n\nAdded /paragraph/ 😀.\n\nSecond italic paragraph with [[https://example.org][a link]].\n#+END_CENTER\n"))
    (dolist (stage '("created" "edited" "native-inserted"))
      (let* ((json-object-type 'alist) (json-array-type 'vector)
             (document (json-read-file (expand-file-name (format "fixtures/live-native-center-%s.json" stage) gdocs-centers-test--directory)))
             (native (seq-filter #'gdocs-convert-center-element-p (gdocs-convert-docs-json-to-ir document)))
             (first (pcase stage ("created" created) ("edited" edited)
                           (_ (string-replace "First *strong*" "First\n\nNative added paragraph 😀 *strong*" edited))))
             (second (if (equal stage "created") "Separate centered block." "*Separate* centered block revised."))
             (expected (concat first "\n#+BEGIN_CENTER\n" second "\n#+END_CENTER\n\n#+BEGIN_CENTER\n#+END_CENTER\n")))
        (should-not (alist-get 'revisionId document))
        (should (= (length native) 3))
        (should (equal (gdocs-convert-ir-to-org native) expected))
        (should-not (gdocs-diff-generate native (gdocs-convert-org-string-to-ir expected)))
        (seq-doseq (element (alist-get 'content (alist-get 'body document)))
          (let* ((paragraph (alist-get 'paragraph element))
                 (text (mapconcat (lambda (part) (or (alist-get 'content (alist-get 'textRun part)) ""))
                                  (alist-get 'elements paragraph) "")))
            (when (string-prefix-p "After" text)
              (should-not (equal (alist-get 'alignment (alist-get 'paragraphStyle paragraph)) "CENTER")))))))))

(provide 'gdocs-centers-test)
;;; gdocs-centers-test.el ends here
