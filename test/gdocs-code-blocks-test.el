;;; gdocs-code-blocks-test.el --- Code/example feature contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; Both block codecs, publication, legacy grouping and neutral diff compose.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gdocs-convert)
(require 'gdocs-diff)

(ert-deftest gdocs-code-blocks-physical-owners ()
  "Code and example semantics are owned outside the facade and diff kernel."
  (dolist (function '(gdocs-convert--src-block-to-ir gdocs-convert--example-block-to-ir
                                                     gdocs-convert--attach-source-block-markers gdocs-convert--attach-example-block-markers
                                                     gdocs-convert--source-block-to-requests gdocs-convert--example-block-to-requests
                                                     gdocs-convert--ir-source-block-to-org gdocs-convert--ir-example-block-to-org
                                                     gdocs-convert--format-src-block-from-lines gdocs-convert--all-code-paragraph-p))
    (should (string-match-p "gdocs-convert-code-blocks\\.elc?\\'" (symbol-file function 'defun))))
  (dolist (function '(gdocs-diff--source-block-key gdocs-diff--example-block-key
                                                   gdocs-diff--source-block-utf16-length gdocs-diff--example-block-utf16-length))
    (should (string-match-p "gdocs-diff-code-blocks\\.elc?\\'" (symbol-file function 'defun)))))

(defun gdocs-code-blocks-test--native (block index)
  "Represent BLOCK as native paragraphs at INDEX using its publication markers."
  (let* ((source-p (eq (plist-get block :type) 'source-block))
         (type (if source-p "src-block" "example-block"))
         (data (if source-p (plist-get block :language) (plist-get block :id)))
         (start index) paragraphs)
    (dolist (line (plist-get block :lines))
      (let ((end (+ index 1 (gdocs-convert--string-to-utf16-length line))))
        (push `((startIndex . ,index) (endIndex . ,end)
                (paragraph . ((elements . [((textRun . ((content . ,(concat line "\n")))))])))) paragraphs)
        (setq index end)))
    (list :content (nreverse paragraphs)
          :marker (cons (intern (format "gdocs-org-marker:%s:block:%s" type data))
                        `((namedRanges . [((ranges . [((startIndex . ,start) (endIndex . ,index))]))])))
          :end index)))

(ert-deftest gdocs-code-blocks-bidirectional-publishing-edits-and-repeat ()
  "Unicode multiline blocks preserve language, markers, indices and no-op diffs."
  (let* ((source "#+BEGIN_SRC emacs-lisp\n(message \"😀\")\n\n42\n#+END_SRC\n\n#+BEGIN_EXAMPLE\nplain 😀\nnext\n#+END_EXAMPLE\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (first (gdocs-code-blocks-test--native (car ir) 1))
         (second (gdocs-code-blocks-test--native (cadr ir) (plist-get first :end)))
         (json (list (cons 'body (list (cons 'content (append (plist-get first :content) (plist-get second :content)))))
                     (cons 'namedRanges (list (plist-get first :marker) (plist-get second :marker)))))
         (remote (gdocs-convert-docs-json-to-ir json))
         (again (gdocs-convert-org-string-to-ir (gdocs-convert-ir-to-org remote))))
    (should (equal (gdocs-diff--element-keys ir) (gdocs-diff--element-keys remote)))
    (should (equal (plist-get (car remote) :language) "emacs-lisp"))
    (should-not (gdocs-diff-generate remote ir))
    (should-not (gdocs-diff-generate remote again))
    (should (gdocs-diff-generate remote (gdocs-convert-org-string-to-ir (string-replace "42" "43" source))))
    (dolist (block ir)
      (let* ((descriptor (gdocs-diff-semantic-element block))
             (plan (gdocs-convert--ir-element-to-requests block 7)))
        (should (equal (plist-get descriptor :key) (gdocs-diff--element-key block)))
        (should (= (plist-get descriptor :body-length) (- (plist-get plan :index) 7)))
        (should (seq-some (lambda (request) (alist-get 'createNamedRange request)) (plist-get plan :requests)))))
    (should (gdocs-convert-ir-to-docs-requests ir))))

(ert-deftest gdocs-code-blocks-legacy-group-composition ()
  "Consecutive unmarked code paragraphs group through the feature adapter."
  (let* ((elements (vector '(:type paragraph :style normal :contents ((:text "😀" :code t)))
                           '(:type paragraph :style normal :contents ((:text "42" :code t)))))
         (group (gdocs-convert-code-block-org-group elements 0)))
    (should (= (plist-get group :next-index) 2))
    (should (equal (plist-get group :org-text) "#+BEGIN_EXAMPLE\n😀\n42\n#+END_EXAMPLE"))
    (should (equal (gdocs-convert-ir-to-org (append elements nil))
                   "#+BEGIN_EXAMPLE\n😀\n42\n#+END_EXAMPLE\n"))))

(defun gdocs-code-blocks-test--feature-branch-p (tree)
  "Return non-nil when TREE recognizes source/example kinds."
  (and (consp tree)
       (or (member tree '('source-block 'example-block))
           (gdocs-code-blocks-test--feature-branch-p (car tree))
           (gdocs-code-blocks-test--feature-branch-p (cdr tree)))))

(ert-deftest gdocs-code-blocks-generic-diff-has-no-feature-branches ()
  "Generic LCS/index logic uses descriptors rather than code block kinds."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "gdocs-diff.el" (file-name-directory (locate-library "gdocs-convert"))))
    (goto-char (point-min))
    (condition-case nil
        (while t (should-not (gdocs-code-blocks-test--feature-branch-p (read (current-buffer)))))
      (end-of-file nil))))

(ert-deftest gdocs-code-blocks-literal-and-empty-lines ()
  "Code retains literal markup, internal empty lines and empty block publication."
  (let* ((source "#+BEGIN_SRC text\n[[status:green][Ship]]\n\n*literal*\n#+END_SRC\n")
         (ir (gdocs-convert-org-string-to-ir source)))
    (should (equal (plist-get (car ir) :lines) '("[[status:green][Ship]]" "" "*literal*")))
    (should (equal (gdocs-convert-ir-to-org ir) source))
    (dolist (type '(source-block example-block))
      (let* ((element (list :type type :lines nil :id "empty"))
             (plan (gdocs-convert--ir-element-to-requests element 4)))
        (should (= (plist-get plan :index) 6))
        (should (= (gdocs-diff--element-utf16-length element) 2))))))

(ert-deftest gdocs-code-blocks-legacy-context-exclusions ()
  "Lists, markers and alignment prevent unmarked-code grouping."
  (dolist (extra '((:list (:type bullet :level 0))
                   (:gdocs-marker ((:type src-block :data "text")))
                   (:alignment center)))
    (let ((paragraph (append '(:type paragraph :style normal :contents ((:text "x" :code t))) extra)))
      (should-not (gdocs-convert-code-block-org-group (vector paragraph) 0)))))

(ert-deftest gdocs-code-blocks-standalone-line-construction ()
  "Code block assembly loads without facade, presentation, transport or accounts."
  (let ((directory (file-name-directory (locate-library "gdocs-convert"))))
    (with-temp-buffer
      (let* ((program '(progn
                         (require 'gdocs-convert-code-blocks)
                         (dolist (feature '(gdocs-convert gdocs-styles gdocs-style-resolve gdocs-sync gdocs-api gdocs-auth))
                           (when (featurep feature) (error "Code block assembly loaded %s" feature)))
                         (let ((paragraph (gdocs-convert--source-block-line-element '(:id "block" :language "text") "😀" 0 1)))
                           (unless (and (eq (plist-get paragraph :style-name) 'gdocs-source-block-single)
                                        (equal (plist-get (car (plist-get paragraph :contents)) :text) "😀"))
                             (error "Standalone line construction changed")))))
             (status (call-process (expand-file-name invocation-name invocation-directory) nil t nil
                                   "--batch" "-Q" "-L" directory "--eval" (prin1-to-string program))))
        (ert-info ((buffer-string)) (should (zerop status)))))))

(ert-deftest gdocs-code-blocks-distinct-panels-have-outside-guards-not-extra-literal-lines ()
  "Every adjacent source/example combination keeps a neutral outside paragraph."
  (require 'gdocs-fidelity-corrections-test)
  (dolist (first '(source-block example-block))
    (dolist (second '(source-block example-block))
      (let* ((local (list (list :type first :language "text" :lines '("  😀" "" "last  ") :id "first")
                          (list :type second :language "text" :lines '("") :id "second")))
             (requests (gdocs-convert-ir-to-docs-requests local))
             (json (gdocs-fidelity-test--returned-json requests))
             (native (gdocs-convert-docs-json-to-ir json))
             (blocks (seq-filter #'gdocs-convert-code-block-element-p native))
             (outside (seq-filter (lambda (request)
                                    (let ((style (alist-get 'updateParagraphStyle request)))
                                      (and (member "shading.backgroundColor" (split-string (or (alist-get 'fields style) "") ","))
                                           (not (alist-get 'shading (alist-get 'paragraphStyle style)))))) requests)))
        (should (= (length outside) 2))
        (should (equal (mapcar (lambda (block) (plist-get block :lines)) blocks) '(("  😀" "" "last  ") (""))))
        (should (= (length blocks) 2))
        (dolist (block local)
          (let ((plan (gdocs-convert--ir-element-to-requests block 9)))
            (should (= (- (plist-get plan :index) 9) (1+ (gdocs-diff-code-blocks--body-length block))))
            (should (= (gdocs-diff--element-utf16-length block) (- (plist-get plan :index) 9)))))
        (should-not (gdocs-diff-generate native local))
        (should-not (gdocs-diff-generate native (gdocs-convert-org-string-to-ir (gdocs-convert-ir-to-org native))))))))

(ert-deftest gdocs-code-blocks-legacy-no-guard-keeps-native-addressing-beside-edits ()
  "Unchanged older literal blocks do not move edits into their following prose."
  (require 'gdocs-fidelity-corrections-test)
  (let* ((source "#+BEGIN_SRC text\nx 😀\n#+END_SRC\n\nAfter *bold*.\n")
         (local (gdocs-convert-org-string-to-ir source))
         (first (gdocs-code-blocks-test--native (car local) 1))
         (start (plist-get first :end))
         (text "After bold.\n")
         (json (list (cons 'body (list (cons 'content
                                             (append (plist-get first :content)
                                                     (list `((startIndex . ,start) (endIndex . ,(+ start (length text)))
                                                             (paragraph . ((elements . [((textRun . ((content . ,text))))])))))))))
                     (cons 'namedRanges (list (plist-get first :marker)))))
         (native (gdocs-convert-docs-json-to-ir json))
         (new (gdocs-convert-org-string-to-ir (string-replace "After *bold*" "Following bold" source)))
         (requests (gdocs-diff-generate native new)))
    (should requests)
    (dolist (request requests)
      (when-let* ((range (alist-get 'range (alist-get 'deleteContentRange request))))
        (should (>= (alist-get 'startIndex range) start))))))

(ert-deftest gdocs-code-blocks-legacy-kept-geometry-anchors-following-list-regrouping ()
  "Adding a list item after an older block uses retained native block width."
  (let* ((source "#+BEGIN_SRC text\nx 😀\n#+END_SRC\n\n- Parent\n")
         (old (gdocs-convert-org-string-to-ir source))
         (new (gdocs-convert-org-string-to-ir (concat source "- Child\n"))))
    (plist-put (car old) :doc-start 1)
    (plist-put (car old) :doc-end 6)
    (plist-put (cadr old) :doc-start 6)
    (plist-put (cadr old) :doc-end 13)
    (let* ((requests (gdocs-diff-generate old new))
           (bullet (car (last (seq-filter (lambda (request) (alist-get 'createParagraphBullets request)) requests))))
           (range (alist-get 'range (alist-get 'createParagraphBullets bullet))))
      (should bullet)
      (should (= (alist-get 'startIndex range) 6)))))

(ert-deftest gdocs-code-blocks-guarded-create-push-edit-delete-and-repeat ()
  "Literal guards survive replacement/deletion without corrupting following prose."
  (require 'gdocs-fidelity-corrections-test)
  (let* ((source "#+BEGIN_SRC text\n  x 😀\n\nlast  \n#+END_SRC\n\n#+BEGIN_EXAMPLE\n*literal*\n#+END_EXAMPLE\n\nAfter *bold* with [[https://example.org][link]].\n")
         (local (gdocs-convert-org-string-to-ir source))
         (sent (gdocs-convert-ir-to-docs-requests local))
         (native (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json sent))))
    (dolist (next (list (string-replace "  x 😀" "  updated 🧭" source)
                        (string-replace "#+BEGIN_SRC text\n  updated 🧭\n\nlast  \n#+END_SRC\n\n" ""
                                        (string-replace "  x 😀" "  updated 🧭" source))))
      (let* ((new (gdocs-convert-org-string-to-ir next))
             (requests (gdocs-diff-generate native new)))
        (should requests)
        (setq sent (append sent requests)
              native (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json sent)))
        (should (equal (gdocs-diff--element-keys native) (gdocs-diff--element-keys new)))
        (should-not (gdocs-diff-generate native new))
        (should-not (gdocs-diff-generate native (gdocs-convert-org-string-to-ir (gdocs-convert-ir-to-org native))))))))

(provide 'gdocs-code-blocks-test)
;;; gdocs-code-blocks-test.el ends here
