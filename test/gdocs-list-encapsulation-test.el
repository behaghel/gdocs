;;; gdocs-list-encapsulation-test.el --- List policy boundary -*- lexical-binding: t; -*-

;;; Commentary:
;; Ownership guards accompany end-to-end list fidelity regressions.

;;; Code:

(require 'ert)
(require 'gdocs-list-conversion-test)

(ert-deftest gdocs-list-encapsulation-source-owner ()
  "Source list syntax and item traversal live in a list feature module."
  (dolist (function '(gdocs-convert--plain-list-positioned gdocs-convert--item-list-type
                                                           gdocs-convert--item-positioned gdocs-convert--org-list-type
                                                           gdocs-convert--top-level-list-p gdocs-convert--list-has-checkboxes-p
                                                           gdocs-convert--strip-continuation-indent))
    (should (string-prefix-p "gdocs-convert-list" (file-name-base (symbol-file function 'defun))))))

(ert-deftest gdocs-list-encapsulation-publication-owner ()
  "Native list grouping, indentation and coordinate repair have a feature owner."
  (dolist (function '(gdocs-convert--list-leading-prefix gdocs-convert--make-list-requests
                                                         gdocs-convert--make-list-indent-request gdocs-convert--fixup-list-nesting
                                                         gdocs-convert--find-list-groups gdocs-convert--alternate-numbered-presets
                                                         gdocs-convert--list-prefixes-before gdocs-convert--guard-non-list-paragraphs))
    (should (string-prefix-p "gdocs-convert-list" (file-name-base (symbol-file function 'defun))))))

(ert-deftest gdocs-list-encapsulation-diff-owner ()
  "List identity, membership and bullet request policy live outside the diff kernel."
  (dolist (function '(gdocs-diff--list-key gdocs-diff--same-list-p gdocs-diff--make-delete-bullets-request))
    (should (equal "gdocs-diff-lists" (file-name-base (symbol-file function 'defun))))))

(defun gdocs-list-encapsulation-test--symbols (file)
  "Read code symbols from FILE without matching strings or comments."
  (with-temp-buffer
    (insert-file-contents file)
    (let (symbols)
      (cl-labels ((walk (form)
                    (cond ((consp form) (walk (car form)) (walk (cdr form)))
                          ((symbolp form) (push form symbols)))))
                 (condition-case nil (while t (walk (read (current-buffer)))) (end-of-file nil)))
      symbols)))

(ert-deftest gdocs-list-encapsulation-neutral-kernels ()
  "Generic conversion and diff cannot inspect list fields or native bullet types."
  (let ((root (file-name-directory (locate-library "gdocs-convert"))))
    (dolist (file '("gdocs-convert.el" "gdocs-diff.el"))
      (let ((symbols (gdocs-list-encapsulation-test--symbols (expand-file-name file root))))
        (dolist (symbol '(:list :level :checked createParagraphBullets deleteParagraphBullets))
          (should-not (memq symbol symbols)))))))

(ert-deftest gdocs-list-encapsulation-description-term-survives ()
  "Description-list terms remain visible rich text and parse back without duplication."
  (let* ((source "- *Term 😀* :: Definition [[https://example.org][link]].\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (expected (gdocs-convert-ir-to-org ir))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (json (gdocs-fidelity-test--returned-json requests))
         (paragraph (alist-get 'paragraph (car (alist-get 'content (alist-get 'body json))))))
    (should (string-match-p (regexp-quote "*Term 😀* ::") expected))
    ;; Explicit synthetic native bullet enrichment: the text/style interpreter
    ;; does not claim to implement native bullet creation.
    (setf (alist-get 'bullet paragraph) '((listId . "descriptive") (nestingLevel . 0))
          (alist-get 'lists json) '((descriptive . ((listProperties . ((nestingLevels . [((glyphSymbol . "●"))])))))))
    (setf (alist-get 'paragraph (car (alist-get 'content (alist-get 'body json)))) paragraph)
    (let ((remote (gdocs-convert-docs-json-to-ir json)))
      (should (equal (gdocs-convert-ir-to-org remote) expected))
      (should-not (gdocs-diff-generate remote (gdocs-convert-org-string-to-ir expected))))))

(ert-deftest gdocs-list-encapsulation-replacement-does-not-clear-nonparagraph ()
  "Replacing a list paragraph with a table must not clear bullets in the new table."
  (let* ((old '(:type paragraph :style normal :list (:type bullet :level 0) :contents ((:text "old"))))
         (new '(:type table :rows ((((:text "cell"))))))
         (result (gdocs-diff--modification-requests old new '(1 . 5))))
    (should-not (seq-some (lambda (request) (alist-get 'deleteParagraphBullets request))
                          (append (plist-get result :insert-reqs) (plist-get result :style-reqs))))))

(ert-deftest gdocs-list-encapsulation-callout-partial-state-is-not-open ()
  "Callout task identity distinguishes partial from open state during push."
  (let* ((source "#+ATTR_CALLOUT: :type note\n#+BEGIN_CALLOUT\n- [ ] Task\n#+END_CALLOUT\n")
         (initial (gdocs-convert-ir-to-docs-requests (gdocs-convert-org-string-to-ir source)))
         (remote (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json initial)))
         (updated (string-replace "[ ]" "[-]" source))
         (new (gdocs-convert-org-string-to-ir updated))
         (edits (gdocs-diff-generate remote new)))
    (should edits)
    (let ((returned (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json (append initial edits)))))
      (should (equal (gdocs-convert-ir-to-org returned) updated))
      (should-not (gdocs-diff-generate returned new)))))

(ert-deftest gdocs-list-encapsulation-neutral-paragraph-contract ()
  "The generic paragraph kernel supports an unrelated opaque physical prefix."
  (let ((element '(:type paragraph :style normal :contents ((:text "body" :bold t)))) emitted)
    (cl-letf (((symbol-function 'gdocs-diff-semantic-paragraph)
               (lambda (_element &optional _old)
                 (list :structure-key "unrelated" :prefix "😀" :compatible nil
                       :requests (lambda (_value start end) (setq emitted (list start end)) nil)))))
             (let* ((result (gdocs-diff--paragraph-content-modification element 7 12))
                    (insert (seq-find (lambda (request) (alist-get 'insertText request)) (plist-get result :insert-reqs)))
                    (bold (seq-find (lambda (request) (eq (alist-get 'bold (alist-get 'textStyle (alist-get 'updateTextStyle request))) t))
                                    (plist-get result :insert-reqs))))
               (should (equal (alist-get 'text (alist-get 'insertText insert)) "😀body"))
               (should (equal emitted '(7 14)))
               (should (= (alist-get 'startIndex (alist-get 'range (alist-get 'updateTextStyle bold))) 9))))))

(ert-deftest gdocs-list-encapsulation-structure-and-style-edits-both-apply ()
  "A simultaneous list structure and styling edit cannot skip native list requests."
  (dolist (change '(style formatting))
    (let* ((old '(:type paragraph :style normal :list (:type bullet :level 0) :contents ((:text "body"))))
           (new (copy-tree old)))
      (plist-put (plist-get new :list) :level 1)
      (if (eq change 'style) (plist-put new :style 'heading-1)
        (plist-put (car (plist-get new :contents)) :bold t))
      (should (seq-some (lambda (request) (alist-get 'createParagraphBullets request))
                        (gdocs-diff-generate (list old) (list new)))))))

(provide 'gdocs-list-encapsulation-test)
;;; gdocs-list-encapsulation-test.el ends here
