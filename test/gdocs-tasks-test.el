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
    (should (equal text "☑\tdone 😀\n◩\tpartial\n☐\topen\n"))
    (should-not (seq-some (lambda (request) (alist-get 'createParagraphBullets request)) requests))
    (should (equal (gdocs-convert-ir-to-org remote) source))
    (should (equal ir before))
    (should-not (gdocs-diff-generate remote ir))
    (should-not (gdocs-diff-generate remote ir))))

(ert-deftest gdocs-tasks-hanging-layout-matches-native-list-columns ()
  "Task glyphs, text and wrapped lines share native bullet columns."
  (dolist (level '(0 1 2))
    (let* ((requests (gdocs-convert-task-run-requests
                      (list :managed-task-prefix (list :level level)) 1))
           (style (alist-get 'paragraphStyle
                             (alist-get 'updateParagraphStyle (car (last requests)))))
           (body (* 36 (1+ level))))
      (should (= (alist-get 'magnitude (alist-get 'indentFirstLine style)) (- body 18)))
      (should (= (alist-get 'magnitude (alist-get 'indentStart style)) body))
      ;; Real API rejects this read-only field with "Unallowed field: tabStops".
      ;; Standard document stops already fall on each 36-point body column.
      (should-not (alist-get 'tabStops style)))))

(ert-deftest gdocs-tasks-tab-separator-keeps-anchor-and-roundtrip ()
  "A tab aligns body text without changing the two-unit prefix or separator anchor."
  (let* ((source "- [ ] task 😀\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (text (mapconcat (lambda (request) (or (alist-get 'text (alist-get 'insertText request)) "")) requests "")))
    (should (equal text "☐\ttask 😀\n"))
    (let ((native (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json requests))))
      (should (equal (gdocs-convert-ir-to-org native) source))
      (should-not (gdocs-diff-generate native ir)))))

(ert-deftest gdocs-tasks-legacy-separator-migrates-once-without-body-loss ()
  "Legacy space-prefixed tasks migrate to tabs without disguising physical text."
  (let* ((source "- [ ] task 😀\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (initial (copy-tree (gdocs-convert-ir-to-docs-requests ir))))
    (dolist (request initial)
      (when-let* ((insert (alist-get 'insertText request)))
        (setf (alist-get 'text insert) (string-replace "☐\t" "☐ " (alist-get 'text insert))))
      (when-let* ((update (alist-get 'updateParagraphStyle request))
                  (style (alist-get 'paragraphStyle update))
                  (_indent (alist-get 'indentFirstLine style)))
        (setf (alist-get 'indentFirstLine style) '((magnitude . 0) (unit . "PT")))))
    (let* ((old (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json initial)))
           (diff (gdocs-diff-generate old ir)))
      (should (equal (gdocs-convert--runs-to-plain-text
                      (plist-get (car (gdocs-diff-tasks-project-ir old)) :contents)) "☐ task 😀"))
      (should diff)
      (let ((returned (gdocs-convert-docs-json-to-ir
                       (gdocs-fidelity-test--returned-json (append initial diff)))))
        (should (equal (gdocs-convert-ir-to-org returned) source))
        (should-not (gdocs-diff-generate returned ir))))))

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
    (should (= (alist-get 'magnitude (alist-get 'indentFirstLine (alist-get 'paragraphStyle last))) 54))
    (should (= (alist-get 'magnitude (alist-get 'indentStart (alist-get 'paragraphStyle last))) 72))
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
         (indent (seq-position requests t (lambda (request _) (string-match-p "indentStart,indentFirstLine" (or (alist-get 'fields (alist-get 'updateParagraphStyle request)) ""))))))
    (should delete)
    (should indent)
    (should (< delete indent))))

(ert-deftest gdocs-tasks-list-boundary-spacing-keeps-items-compact ()
  "Separate list kinds get a visible gap, but items and continuations stay compact."
  (let* ((gdocs-style-definitions
          (append '((gdocs-list-first :paragraph (:space-above 6))
                    (gdocs-list-item :paragraph (:space-above 0))) gdocs-style-definitions))
         (source "1. Number\n\n- [ ] Task\n\n  Continue\n- [X] Next\n\n- Term :: Definition\n")
         (requests (gdocs-convert-ir-to-docs-requests (gdocs-convert-org-string-to-ir source)))
         (json (gdocs-fidelity-test--returned-json requests))
         (paragraphs (alist-get 'content (alist-get 'body json))))
    (should (equal (mapcar (lambda (element)
                             (alist-get 'magnitude (alist-get 'spaceAbove
                                                              (alist-get 'paragraphStyle (alist-get 'paragraph element)))))
                           paragraphs)
                   '(6 6 0 0 6)))))

(ert-deftest gdocs-tasks-state-edit-restores-list-boundary-gap ()
  "Restyling an edited task cannot erase its list's visible boundary gap."
  (let* ((gdocs-style-definitions
          (append '((gdocs-list-first :paragraph (:space-above 6))
                    (gdocs-list-item :paragraph (:space-above 0))) gdocs-style-definitions))
         (source "Before.\n\n- [ ] Task\n- [X] Next\n")
         (old (gdocs-convert-docs-json-to-ir
               (gdocs-fidelity-test--returned-json
                (gdocs-convert-ir-to-docs-requests (gdocs-convert-org-string-to-ir source)))))
         (start (plist-get (cadr old) :doc-start))
         (new (gdocs-convert-org-string-to-ir (string-replace "[ ] Task" "[X] Task" source)))
         (requests (gdocs-diff-generate old new)))
    (should (seq-some (lambda (request)
                        (let ((style (alist-get 'updateParagraphStyle request)))
                          (and (= (or (alist-get 'magnitude (alist-get 'spaceAbove (alist-get 'paragraphStyle style))) -1) 6)
                               (= (alist-get 'startIndex (alist-get 'range style)) start)))) requests))))

(ert-deftest gdocs-tasks-feature-ownership ()
  "Glyph policy and physical projection live outside generic conversion and diff."
  (should (equal "gdocs-convert-tasks" (file-name-base (symbol-file 'gdocs-convert-task-project-paragraph 'defun))))
  (should (equal "gdocs-diff-tasks" (file-name-base (symbol-file 'gdocs-diff-tasks--run 'defun)))))

(provide 'gdocs-tasks-test)
;;; gdocs-tasks-test.el ends here
