;;; gdocs-list-conversion-test.el --- List paragraph fidelity -*- lexical-binding: t; -*-

;;; Commentary:
;; Ordinary nested lists must retain numbering and every authored paragraph.

;;; Code:

(require 'ert)
(require 'gdocs-convert)
(require 'gdocs-diff)
(require 'gdocs-fidelity-corrections-test)

(ert-deftest gdocs-lists-nested-numbering-resumes-parent-counter ()
  "Returning from a numbered child list resumes the parent's counter."
  (let* ((source "1. Parent\n   1. Child\n   2. Child two\n2. Next parent\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (rendered (gdocs-convert-ir-to-org ir)))
    (should (string-match-p "^2\\. Next parent$" rendered))
    (should (string-match-p "^  2\\. Child two$" rendered))))

(ert-deftest gdocs-lists-item-preserves-second-paragraph ()
  "An item cannot silently lose its second authored paragraph before publishing."
  (let* ((source "- First paragraph.\n\n  Second *paragraph* with [[https://example.org][a link]].\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (rendered (gdocs-convert-ir-to-org ir))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (text (mapconcat
                (lambda (request)
                  (or (alist-get 'text (alist-get 'insertText request)) ""))
                requests "")))
    (should (string-match-p "Second" rendered))
    (should (string-match-p "Second" text))
    (should (string-match-p (regexp-quote "*paragraph*") rendered))
    (should (string-match-p (regexp-quote "[[https://example.org][a link]]") rendered))))

(ert-deftest gdocs-lists-task-continuations-full-replay ()
  "Create, native import, Org edit, push and repeats retain one task and its body."
  (let* ((source "- [X] First 😀\n\n  Second *paragraph* [[https://example.org][link]] src_python{1+1}.\n\n  Third paragraph.\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (expected (gdocs-convert-ir-to-org ir))
         (initial (gdocs-convert-ir-to-docs-requests ir))
         (remote (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json initial)))
         (updated (string-replace "1+1" "2+2" (string-replace "Third" "Edited third" expected)))
         (new (gdocs-convert-org-string-to-ir updated))
         (edits (gdocs-diff-generate remote new)))
    (should (= (length ir) 3))
    (should (= (seq-count (lambda (e) (plist-get e :list)) remote) 1))
    (should (= (seq-count #'gdocs-convert-list-continuation-p remote) 2))
    (should (equal (gdocs-convert-ir-to-org remote) expected))
    (should-not (gdocs-diff-generate remote ir))
    (let ((returned (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json (append initial edits)))))
      (should (equal (gdocs-convert-ir-to-org returned) updated))
      (should-not (gdocs-diff-generate returned new))
      (should-not (gdocs-diff-generate returned new)))))

(ert-deftest gdocs-lists-continuation-after-child-list-keeps-source-order ()
  "A parent's continuation follows its children rather than becoming their text."
  (let* ((source "- [ ] Parent\n  - [X] Child\n\n  Parent continuation.\n- [ ] Sibling\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (expected (gdocs-convert-ir-to-org ir))
         (remote (gdocs-convert-docs-json-to-ir
                  (gdocs-fidelity-test--returned-json (gdocs-convert-ir-to-docs-requests ir)))))
    (should (equal (mapcar (lambda (e) (gdocs-convert--runs-to-plain-text (plist-get e :contents))) ir)
                   '("Parent" "Child" "Parent continuation." "Sibling")))
    (should (equal (gdocs-convert-ir-to-org remote) expected))
    (should-not (gdocs-diff-generate remote (gdocs-convert-org-string-to-ir expected)))))

(ert-deftest gdocs-lists-callout-task-continuation-composes ()
  "Callout continuation paragraphs retain rich text and full-width presentation."
  (let* ((source "#+ATTR_CALLOUT: :type note\n#+BEGIN_CALLOUT\n- [X] First\n\n  Second /paragraph/.\n#+END_CALLOUT\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (expected (gdocs-convert-ir-to-org ir))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (remote (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json requests))))
    (should (equal (gdocs-convert-ir-to-org remote) expected))
    (should (= (length (plist-get (car remote) :body-elements)) 2))
    (should-not (gdocs-diff-generate remote ir))))

(ert-deftest gdocs-lists-numbered-continuation-stays-in-one-native-group ()
  "A continuation cannot split a numbered list into restarting native groups."
  (let* ((ir (gdocs-convert-org-string-to-ir "1. First\n\n   Continuation.\n2. Second\n"))
         (_org (gdocs-convert-ir-to-org ir))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (bullets (seq-filter (lambda (r) (alist-get 'createParagraphBullets r)) requests))
         (last-clear (seq-position (reverse requests) t (lambda (r _) (alist-get 'deleteParagraphBullets r)))))
    (should (= (length bullets) 1))
    (should last-clear)
    (should (= (plist-get (plist-get (nth 2 ir) :list) :counter) 2))))

(ert-deftest gdocs-lists-source-nested-numbering-resumes-parent ()
  "Native nested numbered items resume the parent across a continuation paragraph."
  (let* ((ir (gdocs-convert-org-string-to-ir "1. Parent\n   1. Child\n   2. Child two\n\n   Continuation.\n2. Next parent\n"))
         (rendered (gdocs-convert-ir-to-org ir))
         (again (gdocs-convert-org-string-to-ir rendered)))
    (should (string-match-p "^2\\. Next parent$" rendered))
    (should (equal (mapcar (lambda (e) (plist-get (or (plist-get e :list) (plist-get e :list-continuation)) :level)) ir)
                   (mapcar (lambda (e) (plist-get (or (plist-get e :list) (plist-get e :list-continuation)) :level)) again)))))

(ert-deftest gdocs-lists-nested-blocks-cannot-disappear ()
  "Unsupported list body blocks fail before they can be dropped or flattened."
  (should-error (gdocs-convert-org-string-to-ir "- First\n\n  #+BEGIN_SRC python\n  print(1)\n  #+END_SRC\n") :type 'user-error))

(ert-deftest gdocs-lists-continuation-literal-source-retains-edge-spaces ()
  "Structural paragraph trimming must not trim authored literal source text."
  (let* ((source "- [ ] Head\n\n  src_python{  x  }\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (rendered (gdocs-convert-ir-to-org ir)))
    (should (string-match-p (regexp-quote "src_python{  x  }") rendered))))

(ert-deftest gdocs-lists-native-counter-and-continuation-fixture ()
  "Explicit synthetic native lists retain nesting and continuation ownership."
  (let ((position 1) paragraphs markers)
    (dolist (spec '(("Parent\n" 0) ("Child\n" 1) ("Child two\n" 1)
                    ("Continuation.\n" nil) ("Next parent\n" 0)))
      (let* ((text (car spec)) (level (cadr spec)) (end (+ position (length text)))
             (paragraph (list (cons 'elements (vector (list (cons 'textRun (list (cons 'content text)))))))))
        (if level
            (progn
              (push (cons 'bullet (list (cons 'listId "numbers") (cons 'nestingLevel level))) paragraph)
              (push (cons (intern (format "gdocs-org-marker:list-state:head-%d:number:%d" position level))
                          (list (cons 'namedRanges (vector (list (cons 'namedRangeId (format "head-%d" position))
                                                                 (cons 'ranges (vector (list (cons 'startIndex position) (cons 'endIndex end))))))))) markers))
          (push (cons 'gdocs-org-marker:list-continuation:cont:v1:number:0
                      (list (cons 'namedRanges
                                  (vector (list (cons 'namedRangeId "continuation")
                                                (cons 'ranges (vector (list (cons 'startIndex position) (cons 'endIndex end))))))))) markers))
        (push (list (cons 'startIndex position) (cons 'endIndex end) (cons 'paragraph paragraph)) paragraphs)
        (setq position end)))
    (let* ((json (list (cons 'body (list (cons 'content (vconcat (nreverse paragraphs)))))
                       (cons 'namedRanges markers)
                       '(lists . ((numbers . ((listProperties . ((nestingLevels . [((glyphType . "DECIMAL")) ((glyphType . "DECIMAL"))])))))))))
           (ir (gdocs-convert-docs-json-to-ir json))
           (rendered (gdocs-convert-ir-to-org ir)))
      (should (string-match-p "^2\\. Next parent$" rendered))
      (should (equal (mapcar (lambda (e) (plist-get (or (plist-get e :list) (plist-get e :list-continuation)) :counter)) ir)
                     '(1 1 2 1 2)))
      (should-not (gdocs-diff-generate ir (gdocs-convert-org-string-to-ir rendered))))))

(ert-deftest gdocs-lists-invalid-continuation-provenance-fails-closed ()
  "Unknown metadata, orphan continuations and paragraph-spanning ranges block pull."
  (let* ((initial (gdocs-convert-ir-to-docs-requests
                   (gdocs-convert-org-string-to-ir "- [ ] Head\n\n  Body.\n")))
         (json (gdocs-fidelity-test--returned-json initial))
         (entry (seq-find (lambda (entry) (string-prefix-p "gdocs-org-marker:list-continuation:" (symbol-name (car entry))))
                          (alist-get 'namedRanges json))))
    (let ((orphan (copy-tree json)))
      (setf (alist-get 'content (alist-get 'body orphan)) (cdr (alist-get 'content (alist-get 'body orphan))))
      (should-error (gdocs-convert-docs-json-to-ir orphan) :type 'user-error))
    (let* ((bad (copy-tree json))
           (payload (cdr (assq (car entry) (alist-get 'namedRanges bad))))
           (range (car (alist-get 'ranges (car (alist-get 'namedRanges payload))))))
      (setf (alist-get 'startIndex range) 1)
      (should-error (gdocs-convert-docs-json-to-ir bad) :type 'user-error))
    (let* ((bad (copy-tree json)) (ranges (alist-get 'namedRanges bad)))
      (setf (car (assq (car entry) ranges)) 'gdocs-org-marker:list-continuation:cont:v7:check:0)
      (should-error (gdocs-convert-docs-json-to-ir bad) :type 'user-error))))

(ert-deftest gdocs-lists-turning-continuation-into-prose-clears-indent ()
  "Removing item ownership clears both annotation and structural indentation."
  (let* ((initial (gdocs-convert-ir-to-docs-requests
                   (gdocs-convert-org-string-to-ir "- [ ] Head\n\n  Body.\n")))
         (remote (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json initial)))
         (source "- [ ] Head\n\nBody.\n")
         (new (gdocs-convert-org-string-to-ir source))
         (edits (gdocs-diff-generate remote new))
         (json (gdocs-fidelity-test--returned-json (append initial edits)))
         (paragraph (alist-get 'paragraph (cadr (alist-get 'content (alist-get 'body json))))))
    (should (= (alist-get 'magnitude (alist-get 'indentFirstLine (alist-get 'paragraphStyle paragraph))) 0))
    (let ((returned (gdocs-convert-docs-json-to-ir json)))
      (should (equal (gdocs-convert-ir-to-org returned) source))
      (should-not (gdocs-diff-generate returned new)))))

(provide 'gdocs-list-conversion-test)
;;; gdocs-list-conversion-test.el ends here
