;;; gdocs-tasks-test.el --- Managed task glyph fidelity -*- lexical-binding: t; -*-

;;; Commentary:
;; Visible glyphs, not hidden snapshots, carry editable task completion.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gdocs-fidelity-corrections-test)

(ert-deftest gdocs-tasks-publish-and-pull-visible-states ()
  "All Org task states publish as editable text, never native checkbox bullets."
  (let* ((source "- [X] done 😀\n- [-] partial\n- [ ] open\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (before (copy-tree ir))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (text (mapconcat (lambda (request) (or (alist-get 'text (alist-get 'insertText request)) "")) requests ""))
         (remote (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json requests))))
    (should (equal text "☑ done 😀\n◩ partial\n☐ open\n"))
    (should-not (seq-some (lambda (request) (alist-get 'createParagraphBullets request)) requests))
    (should (equal (gdocs-convert-ir-to-org remote) source))
    (should (equal ir before))
    (should-not (gdocs-diff-generate remote ir))
    (should-not (gdocs-diff-generate remote ir))))

(ert-deftest gdocs-tasks-native-glyph-edit-is-authoritative ()
  "A native glyph edit changes Org completion despite unchanged metadata."
  (let* ((source "- [ ] task 😀\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (json (gdocs-fidelity-test--returned-json (gdocs-convert-ir-to-docs-requests ir))))
    (dolist (paragraph (alist-get 'content (alist-get 'body json)))
      (dolist (element (alist-get 'elements (alist-get 'paragraph paragraph)))
        (when-let* ((run (alist-get 'textRun element)))
          (setf (alist-get 'content run) (string-replace "☐" "☑" (alist-get 'content run))))))
    (let* ((remote (gdocs-convert-docs-json-to-ir json))
           (expected "- [X] task 😀\n"))
      (should (equal (gdocs-convert-ir-to-org remote) expected))
      (should-not (gdocs-diff-generate remote (gdocs-convert-org-string-to-ir expected))))))

(ert-deftest gdocs-tasks-push-state-edits-replace-annotations ()
  "State edits use physical glyph indices and replace native range annotations once."
  (let* ((old (gdocs-convert-org-string-to-ir "- [ ] task\n"))
         (remote (gdocs-convert-docs-json-to-ir
                  (gdocs-fidelity-test--returned-json (gdocs-convert-ir-to-docs-requests old))))
         (new (gdocs-convert-org-string-to-ir "- [X] task\n"))
         (requests (gdocs-diff-generate remote new)))
    (should (alist-get 'deleteNamedRange (car requests)))
    (should (= (seq-count (lambda (request) (alist-get 'createNamedRange request)) requests) 1))
    (should (seq-some (lambda (request) (equal (alist-get 'text (alist-get 'insertText request)) "☑")) requests))))

(ert-deftest gdocs-tasks-literal-glyphs-are-not-inferred ()
  "Unmarked glyph text stays prose, while task body prefixes stay literal."
  (let* ((source "- [ ] ☑ literal body\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (remote (gdocs-convert-docs-json-to-ir
                  (gdocs-fidelity-test--returned-json (gdocs-convert-ir-to-docs-requests ir)))))
    (should (equal (gdocs-convert-ir-to-org remote) source)))
  (let ((ir (gdocs-convert-docs-json-to-ir
             '((body . ((content . [((startIndex . 1) (endIndex . 9)
                                     (paragraph . ((elements . [((textRun . ((content . "☑ prose\n"))))]))))])))))))
    (should-not (plist-get (car ir) :list))))

(ert-deftest gdocs-tasks-unknown-native-checkbox-state-fails-closed ()
  "Legacy task provenance cannot substitute for unreadable native completion."
  (let ((json '((body . ((content . [((startIndex . 1) (endIndex . 6)
                                      (paragraph . ((bullet . ((listId . "opaque") (nestingLevel . 0)))
                                                    (elements . [((textRun . ((content . "task\n"))))]))))])))
                (namedRanges . ((gdocs-org-marker:list-state:task:check:0 . ((namedRanges . [((name . "gdocs-org-marker:list-state:task:check:0")
                                                                                              (namedRangeId . "legacy-task")
                                                                                              (ranges . [((startIndex . 1) (endIndex . 6))]))])))))
                (lists . ((opaque . ((listProperties . ((nestingLevels . [((glyphType . "GLYPH_TYPE_UNSPECIFIED"))]))))))))))
    (dolist (candidate (list json (assq-delete-all 'lists (copy-tree json))))
      (should-error (gdocs-convert-docs-json-to-ir candidate) :type 'user-error))))

(ert-deftest gdocs-tasks-nested-rich-body-and-native-state-edits ()
  "Nesting, UTF-16 spans, links, inline source and authored emphasis round-trip."
  (let* ((source "- [ ] parent 😀\n  - [X] *bold* [[https://example.org][link]] src_python{1+1}\n  - [-] partial\n- [ ] next\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (remote (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json requests))))
    (should (equal (gdocs-convert-ir-to-org remote) source))
    (should-not (gdocs-diff-generate remote ir))
    (should (= (gdocs-diff--element-utf16-length (car ir))
               (- (plist-get (car remote) :doc-end) (plist-get (car remote) :doc-start))))))

(ert-deftest gdocs-tasks-callout-state-edit-does-not-use-hidden-snapshot ()
  "Callout metadata preserves structure, but visible glyph edits own completion."
  (let* ((source "#+ATTR_CALLOUT: :type info :title \"Résumé 😀\"\n#+BEGIN_CALLOUT\n- [ ] task\n- [-] partial\n\nSecond paragraph.\n#+END_CALLOUT\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (json (gdocs-fidelity-test--returned-json requests)))
    (dolist (paragraph (alist-get 'content (alist-get 'body json)))
      (dolist (element (alist-get 'elements (alist-get 'paragraph paragraph)))
        (when-let* ((run (alist-get 'textRun element)))
          (setf (alist-get 'content run) (string-replace "☐" "☑" (alist-get 'content run))))))
    (let* ((remote (gdocs-convert-docs-json-to-ir json))
           (expected (string-replace "[ ] task" "[X] task" source)))
      (should (equal (gdocs-convert-ir-to-org remote) expected))
      (should-not (gdocs-diff-generate remote (gdocs-convert-org-string-to-ir expected)))
      (should (= (gdocs-diff--element-utf16-length (car ir))
                 (- (plist-get (car remote) :doc-end) (plist-get (car remote) :doc-start)))))))

(ert-deftest gdocs-tasks-state-and-inline-source-push-replay ()
  "Create, edit, push and repeated push preserve glyph and source annotations once."
  (let* ((old (gdocs-convert-org-string-to-ir "- [ ] 😀 src_python{1+1}\n"))
         (initial (gdocs-convert-ir-to-docs-requests old))
         (remote (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json initial)))
         (source "- [X] 😀 src_python{1+2}\n")
         (new (gdocs-convert-org-string-to-ir source))
         (edits (gdocs-diff-generate remote new)))
    (should (= (seq-count (lambda (request) (alist-get 'createNamedRange request)) edits) 2))
    (let ((returned (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json (append initial edits)))))
      (should (equal (gdocs-convert-ir-to-org returned) source))
      (should-not (gdocs-diff-generate returned new))
      (should-not (gdocs-diff-generate returned new)))))

(ert-deftest gdocs-tasks-annotation-anchors-separator-not-replaced-glyph ()
  "The separator survives glyph replacement and identifies a removed glyph safely."
  (let* ((ir (gdocs-convert-org-string-to-ir "- [ ] task\n"))
         (initial (gdocs-convert-ir-to-docs-requests ir))
         (marker (seq-find (lambda (request) (alist-get 'createNamedRange request)) initial))
         (range (alist-get 'range (alist-get 'createNamedRange marker))))
    (should (equal range '((startIndex . 2) (endIndex . 3))))
    (should-error
     (gdocs-convert-docs-json-to-ir
      (gdocs-fidelity-test--returned-json
       (append initial '(((deleteContentRange . ((range . ((startIndex . 1) (endIndex . 2))))))))))
     :type 'user-error)))

(ert-deftest gdocs-tasks-mixed-native-lists-repair-task-indent-last ()
  "Native bullets and their guards cannot overwrite nested managed task indentation."
  (let* ((ir (gdocs-convert-org-string-to-ir "- bullet\n  - nested bullet\n  - [X] task\n"))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (last (alist-get 'updateParagraphStyle (car (last requests)))))
    (should last)
    (should (= (alist-get 'magnitude (alist-get 'indentFirstLine (alist-get 'paragraphStyle last))) 36))
    (should (= (alist-get 'startIndex (alist-get 'range last)) 22))))

(ert-deftest gdocs-tasks-converting-native-bullet-clears-control-before-glyph-style ()
  "A bullet-to-task edit removes native list membership before task presentation."
  (let* ((old (gdocs-convert-docs-json-to-ir
               '((body . ((content . [((startIndex . 1) (endIndex . 6)
                                       (paragraph . ((bullet . ((listId . "ordinary") (nestingLevel . 0)))
                                                     (elements . [((textRun . ((content . "task\n"))))]))))])))
                 (lists . ((ordinary . ((listProperties . ((nestingLevels . [((glyphSymbol . "●"))]))))))))))
         (new (gdocs-convert-org-string-to-ir "- [X] task\n"))
         (requests (gdocs-diff-generate old new))
         (delete (seq-position requests t (lambda (request _) (alist-get 'deleteParagraphBullets request))))
         (indent (seq-position requests t (lambda (request _) (equal (alist-get 'fields (alist-get 'updateParagraphStyle request)) "indentStart,indentFirstLine")))))
    (should delete)
    (should indent)
    (should (< delete indent))))

(ert-deftest gdocs-tasks-feature-ownership ()
  "Glyph policy and physical projection live outside generic conversion and diff."
  (should (equal "gdocs-convert-tasks" (file-name-base (symbol-file 'gdocs-convert-task-project-paragraph 'defun))))
  (should (equal "gdocs-diff-tasks" (file-name-base (symbol-file 'gdocs-diff-tasks--run 'defun)))))

(provide 'gdocs-tasks-test)
;;; gdocs-tasks-test.el ends here
