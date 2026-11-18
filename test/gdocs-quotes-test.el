;;; gdocs-quotes-test.el --- Quote feature contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; Quote conversion, chrome, authored runs and diff compose through the facade.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'gdocs-convert)
(require 'gdocs-diff)

(defconst gdocs-quotes-test--directory (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the native quote captures.")

(ert-deftest gdocs-quotes-live-captures-preserve-authored-emphasis-and-boundaries ()
  "Both live presentation bases decode consistently after create, edit and toggle."
  (let* ((created "#+BEGIN_QUOTE\nPlain /emphasized 😀/ text.\n\nsecond paragraph with *bold* and [[https://example.org][a link]].\n#+END_QUOTE\n\n#+BEGIN_QUOTE\nA separate quote.\n#+END_QUOTE\n")
         (edited "#+BEGIN_QUOTE\nPlain emphasized 😀 /changed text/.\n\nsecond paragraph with *strong 😀* and [[https://example.org][a link]].\n#+END_QUOTE\n\n#+BEGIN_QUOTE\nAnother separate quote.\n#+END_QUOTE\n"))
    (dolist (mode '("italic" "upright"))
      (dolist (stage '("created" "edited" "native-toggled"))
        (let* ((json-object-type 'alist) (json-array-type 'vector)
               (document (json-read-file (expand-file-name (format "fixtures/live-native-quote-%s-%s.json" mode stage) gdocs-quotes-test--directory)))
               (expected (pcase stage ("created" created) ("edited" edited)
                                (_ (string-replace "/changed text/" "changed text" edited)))))
          (should-not (alist-get 'revisionId document))
          (dolist (local-base '(nil t))
            (let* ((gdocs-style-definitions
                    (append (mapcar (lambda (role) (list role :text (list :italic local-base)))
                                    '(gdocs-quote-block gdocs-quote-block-first gdocs-quote-block-line gdocs-quote-block-last gdocs-quote-block-single))
                            gdocs-style-definitions))
                   (native (seq-filter #'gdocs-convert-quote-element-p (gdocs-convert-docs-json-to-ir document))))
              (should (= (length native) 2))
              (should (= (length (plist-get (car native) :paragraphs)) 2))
              (should (equal (gdocs-convert-ir-to-org native) expected))
              (should-not (gdocs-diff-generate native (gdocs-convert-org-string-to-ir expected))))))))))

(ert-deftest gdocs-quotes-live-captures-without-provenance-fail-closed ()
  "Removing an actual applied-base snapshot cannot cause guessed emphasis."
  (dolist (mode '("italic" "upright"))
    (let* ((json-object-type 'alist) (json-array-type 'vector)
           (document (json-read-file (expand-file-name (format "fixtures/live-native-quote-%s-created.json" mode) gdocs-quotes-test--directory)))
           (changed 0))
      (dolist (entry (alist-get 'namedRanges document))
        (let ((name (symbol-name (car entry))))
          (when (string-prefix-p "gdocs-org-marker:quote-block:" name)
            (setcar entry (intern (replace-regexp-in-string ":italic-v1-[01]$" "" name)))
            (cl-incf changed))))
      (should (> changed 0))
      (should-error (gdocs-convert-docs-json-to-ir document) :type 'user-error))))

(ert-deftest gdocs-quotes-physical-owners ()
  "Quote policy is not left in generic traversal or diff algorithms."
  (dolist (function '(gdocs-convert--quote-block-to-ir gdocs-convert--block-paragraph-runs
                                                       gdocs-convert--ir-quote-block-to-org gdocs-convert--attach-quote-block-markers
                                                       gdocs-convert--strip-quote-visual-style gdocs-convert--quote-block-marker
                                                       gdocs-convert--quote-block-to-requests gdocs-convert--quote-block-paragraph-element
                                                       gdocs-convert--paragraph-style))
    (should (string-match-p "gdocs-convert-quotes\\.elc?\\'" (symbol-file function 'defun))))
  (dolist (function '(gdocs-diff--quote-block-key gdocs-diff--quote-block-utf16-length))
    (should (string-match-p "gdocs-diff-quotes\\.elc?\\'" (symbol-file function 'defun))))
  (should (string-match-p "gdocs-convert-paragraphs\\.elc?\\'"
                          (symbol-file 'gdocs-convert--consume-generated-separator 'defun))))

(defun gdocs-quotes-test--native ()
  "Return a synthetic marked quote with rich text, Unicode and separator chrome."
  (json-parse-string
   (concat "{\"body\":{\"content\":["
           "{\"startIndex\":1,\"endIndex\":9,\"paragraph\":{\"elements\":[{\"textRun\":{\"content\":\"Bold 😀\\n\",\"textStyle\":{\"italic\":true,\"bold\":true}}}]}},"
           "{\"startIndex\":9,\"endIndex\":14,\"paragraph\":{\"elements\":[{\"textRun\":{\"content\":\"Link\\n\",\"textStyle\":{\"italic\":true,\"link\":{\"url\":\"https://example.org\"}}}}]}},"
           "{\"startIndex\":14,\"endIndex\":15,\"paragraph\":{\"elements\":[{\"textRun\":{\"content\":\"\\n\"}}]}}]},"
           "\"namedRanges\":{\"gdocs-org-marker:quote-block:quoted:italic-v1-1\":{\"namedRanges\":[{\"ranges\":[{\"startIndex\":1,\"endIndex\":14}]}]}}}")
   :object-type 'alist :array-type 'list :null-object nil :false-object nil))

(ert-deftest gdocs-quotes-bidirectional-publication-and-repeat ()
  "Rich quotes retain authored bold/links, Unicode addressing and stable plans."
  (let* ((source "#+BEGIN_QUOTE\n*Bold 😀*\n\n[[https://example.org][Link]]\n#+END_QUOTE\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (remote (gdocs-convert-docs-json-to-ir (gdocs-quotes-test--native)))
         (returned (gdocs-convert-ir-to-org remote))
         (again (gdocs-convert-org-string-to-ir returned))
         (descriptor (gdocs-diff-semantic-element (car ir)))
         (plan (gdocs-convert--quote-block-to-requests (car ir) 1)))
    (should (equal returned source))
    (should (= (plist-get (car remote) :doc-end) 15))
    (should (equal (plist-get descriptor :key) (gdocs-diff--element-key (car ir))))
    (should (= (plist-get descriptor :body-length) 14))
    (should (= (plist-get plan :index) 15))
    (should-not (gdocs-diff-generate remote ir))
    (should-not (gdocs-diff-generate remote again))
    (should (gdocs-diff-generate remote (gdocs-convert-org-string-to-ir (string-replace "Bold" "Changed" source))))
    (should (gdocs-convert-ir-to-docs-requests ir))))

(defun gdocs-quotes-test--branch-p (tree)
  "Return non-nil if TREE contains quote-kind recognition."
  (and (consp tree)
       (or (equal tree ''quote-block)
           (gdocs-quotes-test--branch-p (car tree))
           (gdocs-quotes-test--branch-p (cdr tree)))))

(ert-deftest gdocs-quotes-generic-diff-neutrality ()
  "Generic diff receives descriptors instead of inspecting quote kinds."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "gdocs-diff.el" (file-name-directory (locate-library "gdocs-convert"))))
    (goto-char (point-min))
    (condition-case nil
        (while t (should-not (gdocs-quotes-test--branch-p (read (current-buffer)))))
      (end-of-file nil))))

(ert-deftest gdocs-quotes-native-boundaries-and-separator-ownership ()
  "Native paragraphs and quote IDs retain boundaries; separator ownership is exact."
  (let* ((runs '((:text "Hello" :italic t) (:text "world" :italic t) (:text "again" :italic t)))
         (before (copy-tree runs))
         (paragraphs (cl-loop for run in runs for id in '("one" "one" "two")
                              for start in '(1 7 13) for end in '(7 13 19)
                              collect (list :type 'paragraph :style 'normal :contents (list run)
                                            :gdocs-marker (list (list :type 'quote-block :id id :data "italic-v1-1"))
                                            :doc-start start :doc-end end)))
         (tail '(:type paragraph :style normal :contents ((:text "After"))))
         (ir (append paragraphs (list '(:type paragraph :contents nil :doc-start 19 :doc-end 20) tail)))
         (grouped (gdocs-convert--attach-quote-block-markers ir)))
    (should (= (length grouped) 3))
    (should (equal (mapcar #'gdocs-convert--runs-to-plain-text (plist-get (car grouped) :paragraphs))
                   '("Hello" "world")))
    (should (equal (mapcar #'gdocs-convert--runs-to-plain-text (plist-get (cadr grouped) :paragraphs)) '("again")))
    (should (= (plist-get (cadr grouped) :doc-end) 20))
    (should (equal (caddr grouped) tail))
    (should (equal runs before))))

(ert-deftest gdocs-quotes-empty-publication-and-style-revision ()
  "Empty quote planning retains content newline and chrome; style revisions matter."
  (let* ((block '(:type quote-block :paragraphs nil :id "empty"))
         (plan (gdocs-convert--quote-block-to-requests block 4)))
    (should (= (plist-get plan :index) 6))
    (should (= (gdocs-diff--element-utf16-length block) 2))
    (should-not (equal (gdocs-diff--element-key block)
                       (gdocs-diff--element-key (plist-put (copy-tree block) :style-revision "new"))))))

(ert-deftest gdocs-quotes-standalone-paragraph-construction ()
  "Quote assembly loads without facade, presentation, transport or accounts."
  (let ((directory (file-name-directory (locate-library "gdocs-convert"))))
    (with-temp-buffer
      (let* ((program '(progn
                         (require 'gdocs-convert-quotes)
                         (dolist (feature '(gdocs-convert gdocs-styles gdocs-style-resolve gdocs-sync gdocs-api gdocs-auth))
                           (when (featurep feature) (error "Quote assembly loaded %s" feature)))
                         (let ((paragraph (gdocs-convert--quote-block-paragraph-element
                                           '(:id "quote") '((:text "😀" :bold t)) 0 1)))
                           (unless (and (eq (plist-get paragraph :style-name) 'gdocs-quote-block-single)
                                        (plist-get (car (plist-get paragraph :contents)) :bold))
                             (error "Standalone quote assembly changed")))))
             (status (call-process (expand-file-name invocation-name invocation-directory) nil t nil
                                   "--batch" "-Q" "-L" directory "--eval" (prin1-to-string program))))
        (ert-info ((buffer-string)) (should (zerop status)))))))

(provide 'gdocs-quotes-test)
;;; gdocs-quotes-test.el ends here
