;;; gdocs-section-breaks-test.el --- Section break fidelity -*- lexical-binding: t; -*-

;;; Commentary:
;; Section breaks preserve semantic identity independently of their visual rule.

;;; Code:

(require 'ert)
(require 'json)
(require 'gdocs-fidelity-corrections-test)
(require 'gdocs-sync)

(defconst gdocs-section-breaks-test--directory (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing synthetic section-break fixtures.")

(defun gdocs-section-breaks-test--native (requests)
  "Replay REQUESTS and normalize native IR exactly as sync does."
  (gdocs-sync--filter-empty-paragraphs
   (gdocs-sync--filter-title
    (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json requests)))))

(ert-deftest gdocs-section-breaks-create-pull-and-repeat ()
  "A visible break retains identity through create, pull and unchanged push."
  (let* ((source "Before *bold* 😀.\n\n-----\nAfter /italic/ with [[https://example.org][link]].\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (native (gdocs-section-breaks-test--native requests)))
    (should (seq-some (lambda (request)
                        (let ((border (alist-get 'borderBottom (alist-get 'paragraphStyle (alist-get 'updateParagraphStyle request)))))
                          (> (or (alist-get 'magnitude (alist-get 'width border)) 0) 0))) requests))
    (should (equal (gdocs-convert-ir-to-org native) source))
    (should (= (seq-count (lambda (element) (eq (plist-get element :type) 'horizontal-rule)) native) 1))
    (should-not (gdocs-diff-generate native ir))))

(ert-deftest gdocs-section-breaks-adjacent-and-terminal-breaks-remain-distinct ()
  "Adjacent breaks and a terminal break remain independently recoverable."
  (dolist (source '("-----\n" "-----\n\n-----\n" "Before.\n\n-----\n"))
    (let* ((ir (gdocs-convert-org-string-to-ir source))
           (native (gdocs-section-breaks-test--native (gdocs-convert-ir-to-docs-requests ir))))
      (should (equal (gdocs-convert-ir-to-org native) source))
      (should-not (gdocs-diff-generate native ir)))))

(ert-deftest gdocs-section-breaks-edits-preserve-following-utf16-geometry ()
  "Insertions and deletions preserve breaks, formatting and following emoji."
  (let* ((source "Before 😀.\n\n-----\nMiddle.\n\n-----\nAfter 😀.\n")
         (initial (gdocs-convert-ir-to-docs-requests (gdocs-convert-org-string-to-ir source)))
         (native (gdocs-section-breaks-test--native initial))
         (updated "Before *revised* 😀.\n\n-----\nMiddle /revised/.\n\n-----\n\n-----\nAfter changed 😀.\n")
         (new (gdocs-convert-org-string-to-ir updated))
         (requests (gdocs-diff-generate native new))
         (result (gdocs-section-breaks-test--native (append initial requests))))
    (should requests)
    (should (equal (gdocs-convert-ir-to-org result) updated))
    (should-not (gdocs-diff-generate result new))
    (let* ((removed (gdocs-convert-org-string-to-ir "Before *revised* 😀.\n\nAfter changed 😀.\n"))
           (final (gdocs-section-breaks-test--native (append initial requests (gdocs-diff-generate result removed)))))
      (should (equal (gdocs-convert-ir-to-org final) "Before *revised* 😀.\n\nAfter changed 😀.\n"))
      (should-not (gdocs-diff-generate final removed)))))

(ert-deftest gdocs-section-breaks-native-horizontal-rule-object-is-recoverable ()
  "Native rule objects preserve identity without border or marker inference."
  (let* ((document '((body . ((content . [((startIndex . 1) (endIndex . 3)
                                           (paragraph . ((elements . [((horizontalRule)) ((textRun . ((content . "\n"))))]))))
                                          ((startIndex . 3) (endIndex . 13)
                                           (paragraph . ((paragraphStyle . ((namedStyleType . "NORMAL_TEXT")))
                                                         (elements . [((textRun . ((content . "After 😀.\n"))))]))))])))))
         (native (gdocs-convert-docs-json-to-ir document))
         (source "-----\nAfter 😀.\n"))
    (should (eq (plist-get (car native) :type) 'horizontal-rule))
    (should (= (gdocs-diff--element-utf16-length (car native)) 2))
    (should (equal (gdocs-convert-ir-to-org native) source))
    (should-not (gdocs-diff-generate native (gdocs-convert-org-string-to-ir source)))))

(ert-deftest gdocs-section-breaks-invalid-marker-context-fails-closed ()
  "Unsupported provenance and authored text on separators cannot disappear."
  (let ((initial (gdocs-fidelity-test--returned-json
                  (gdocs-convert-ir-to-docs-requests (gdocs-convert-org-string-to-ir "-----\n")))))
    (dolist (kind '(version empty-range partial text bullet overlap))
      (let* ((document (copy-tree initial t))
             (entry (car (alist-get 'namedRanges document)))
             (range-entry (elt (alist-get 'namedRanges (cdr entry)) 0))
             (paragraph (alist-get 'paragraph (seq-find (lambda (element) (equal (alist-get 'startIndex element) 1))
                                                        (alist-get 'content (alist-get 'body document))))))
        (pcase kind
          ('version (setcar entry (intern (string-remove-suffix ":v1" (symbol-name (car entry))))))
          ('empty-range (setf (alist-get 'ranges range-entry) []))
          ('partial (setf (alist-get 'endIndex (elt (alist-get 'ranges range-entry) 0)) 3))
          ('text (setf (alist-get 'content (alist-get 'textRun (elt (alist-get 'elements paragraph) 0))) "authored\n"))
          ('bullet (nconc paragraph '((bullet . ((listId . "list"))))))
          ('overlap
           (setf (alist-get 'namedRanges document)
                 (append (alist-get 'namedRanges document)
                         (list (cons 'gdocs-org-marker:unknown:overlap:v1 (copy-tree (cdr entry) t)))))))
        (ert-info ((format "Invalid break context: %S" kind))
                  (should-error (gdocs-convert-docs-json-to-ir document) :type 'user-error))))))

(ert-deftest gdocs-section-breaks-presentation-does-not-establish-identity ()
  "Borders are neither necessary nor sufficient for marked break identity."
  (let* ((initial (gdocs-fidelity-test--returned-json
                   (gdocs-convert-ir-to-docs-requests (gdocs-convert-org-string-to-ir "-----\n")))))
    (seq-doseq (element (alist-get 'content (alist-get 'body initial)))
      (when-let* ((paragraph (alist-get 'paragraph element)))
        (setf (alist-get 'paragraphStyle paragraph) nil)))
    (let ((native (gdocs-sync--filter-empty-paragraphs (gdocs-convert-docs-json-to-ir initial))))
      (should (equal (gdocs-convert-ir-to-org native) "-----\n"))
      (should-not (gdocs-diff-generate native (gdocs-convert-org-string-to-ir "-----\n"))))
    (setf (alist-get 'namedRanges initial) nil)
    (should-not (gdocs-sync--filter-empty-paragraphs (gdocs-convert-docs-json-to-ir initial)))))

(ert-deftest gdocs-section-breaks-style-is-configurable-and-outside-border-is-cleared ()
  "Logical style controls the rule; no visible border is lent to following prose."
  (let* ((gdocs-style-definitions
          (cons '(gdocs-section-break :parent normal :paragraph (:borders ((bottom :width 2 :padding 0))) :text (:font-size 1))
                gdocs-style-definitions))
         (plan (gdocs-convert--horizontal-rule-to-requests 7))
         (requests (plist-get plan :requests)))
    (should (= (plist-get plan :index) 9))
    (dolist (entry '((7 . 2) (8 . 0)))
      (should (seq-some (lambda (request)
                          (let* ((update (alist-get 'updateParagraphStyle request))
                                 (border (alist-get 'borderBottom (alist-get 'paragraphStyle update))))
                            (and (equal (alist-get 'startIndex (alist-get 'range update)) (car entry)) border
                                 (= (alist-get 'magnitude (alist-get 'width border)) (cdr entry))))) requests)))))

(ert-deftest gdocs-section-breaks-unsupported-nesting-is-rejected ()
  "A break inside a richer unsupported enclosing block does not get flattened."
  (dolist (source '("#+BEGIN_QUOTE\n-----\n#+END_QUOTE\n"
                    "#+BEGIN_CENTER\n-----\n#+END_CENTER\n"
                    "- item\n\n  -----\n"))
    (should-error (gdocs-convert-org-string-to-ir source) :type 'user-error)))

(ert-deftest gdocs-section-breaks-feature-owns-conversion-and-diff-policy ()
  "Recognition, requests, identity and geometry have dedicated feature owners."
  (dolist (function '(gdocs-convert-section-break-source gdocs-convert--horizontal-rule-to-requests
                                                         gdocs-convert-section-break-to-org gdocs-convert-section-break-validate-native-context))
    (should (string-match-p "gdocs-convert-section-breaks\\.elc?\\'" (symbol-file function 'defun))))
  (should (string-match-p "gdocs-diff-section-breaks\\.elc?\\'" (symbol-file 'gdocs-diff-section-breaks--element 'defun)))
  (cl-labels ((recognizes (tree) (and (consp tree) (or (equal tree ''horizontal-rule)
                                                       (recognizes (car tree)) (recognizes (cdr tree))))))
             (with-temp-buffer
               (insert-file-contents (expand-file-name "gdocs-diff.el" (file-name-directory (locate-library "gdocs-convert"))))
               (goto-char (point-min))
               (condition-case nil (while t (should-not (recognizes (read (current-buffer))))) (end-of-file nil)))))

(ert-deftest gdocs-section-breaks-native-missing-outside-guard-uses-physical-length ()
  "User deletion of the outside blank paragraph cannot shift following text."
  (let* ((source "-----\nAfter 😀.\n")
         (initial (gdocs-convert-ir-to-docs-requests (gdocs-convert-org-string-to-ir source)))
         (native (gdocs-section-breaks-test--native
                  (append initial '(((deleteContentRange . ((range . ((startIndex . 2) (endIndex . 3)))))))))))
    (should (= (gdocs-diff--element-utf16-length (car native)) 1))
    (should (equal (gdocs-convert-ir-to-org native) source))
    (should-not (gdocs-diff-generate native (gdocs-convert-org-string-to-ir source)))))

(ert-deftest gdocs-section-breaks-standalone-feature-loading-is-neutral ()
  "Feature recognition and rendering load without facade, styles or transport."
  (with-temp-buffer
    (let* ((program '(progn
                       (require 'gdocs-convert-section-breaks)
                       (dolist (feature '(gdocs-convert gdocs-styles gdocs-style-resolve gdocs-sync gdocs-api gdocs-auth))
                         (when (featurep feature) (error "Section break loaded %s" feature)))
                       (unless (equal (gdocs-convert-section-break-to-org '(:type horizontal-rule)) "-----")
                         (error "Standalone section break rendering changed"))))
           (status (call-process (expand-file-name invocation-name invocation-directory) nil t nil
                                 "--batch" "-Q" "-L" (file-name-directory (locate-library "gdocs-convert"))
                                 "--eval" (prin1-to-string program))))
      (ert-info ((buffer-string)) (should (zerop status))))))

(ert-deftest gdocs-section-breaks-standard-fixture-create-edit-and-repeat ()
  "Fixture-level publication preserves rich prose and adjacent/terminal breaks."
  (let* ((source (with-temp-buffer
                   (insert-file-contents (expand-file-name "test/fixtures/disposable-section-break-conformance.org"
                                                           (file-name-directory (locate-library "gdocs-convert"))))
                   (buffer-string)))
         (initial (gdocs-convert-ir-to-docs-requests (gdocs-convert-org-string-to-ir source)))
         (native (gdocs-section-breaks-test--native initial))
         (updated (string-replace "Before *bold* prose 😀." "Before *revised* prose 😀."
                                  (string-replace "After final prose 😀." "After final /revised/ prose 😀." source)))
         (new (gdocs-convert-org-string-to-ir updated))
         (result (gdocs-section-breaks-test--native (append initial (gdocs-diff-generate native new)))))
    (should (equal (gdocs-convert-ir-to-org native) source))
    (should-not (gdocs-diff-generate native (gdocs-convert-org-string-to-ir source)))
    (should (equal (gdocs-convert-ir-to-org result) updated))
    (should-not (gdocs-diff-generate result new))))

(ert-deftest gdocs-section-breaks-native-rule-inside-table-fails-closed ()
  "A recognized native rule in unsupported table-cell prose cannot disappear."
  (let* ((paragraph '((paragraph . ((elements . [((horizontalRule))])))))
         (cell (list (cons 'content (list paragraph))))
         (row (list (cons 'tableCells (list cell))))
         (table (list (cons 'table (list (cons 'tableRows (list row)))))))
    (should-error (gdocs-convert--structural-element-to-ir table nil nil) :type 'user-error)))

(ert-deftest gdocs-section-breaks-live-captures-preserve-breaks-and-rich-prose ()
  "Actual create/delete/insert responses preserve transitions and surrounding prose."
  (let ((created "Before *bold* prose 😀.\n\n-----\nMiddle /italic/ prose with [[https://example.org][a link]].\n\n-----\n\n-----\nAfter final prose 😀.\n\n-----\n")
        (deleted "Before *revised* prose 😀.\n\n-----\nMiddle italic prose with [[https://example.org][a link]].\n\n-----\nAfter final *revised* prose 😀.\n\n-----\nNew final prose 😀.\n"))
    (dolist (stage '("created" "deleted" "edited"))
      (let* ((json-object-type 'alist) (json-array-type 'vector)
             (document (json-read-file (expand-file-name (format "fixtures/live-section-break-%s.json" stage)
                                                         gdocs-section-breaks-test--directory)))
             ;; The personal heading is outside this semantic feature's contract.
             (native (seq-remove (lambda (element) (eq (plist-get element :style) 'heading-1))
                                 (gdocs-sync--filter-empty-paragraphs
                                  (gdocs-sync--filter-title (gdocs-convert-docs-json-to-ir document)))))
             (expected (pcase stage ("created" created) ("deleted" deleted) (_ (concat deleted "\n-----\n")))))
        (should-not (alist-get 'revisionId document))
        (should (= (seq-count #'gdocs-convert-section-break-element-p native) (if (equal stage "deleted") 3 4)))
        (should (equal (gdocs-convert-ir-to-org native)
                       (if (equal stage "created") expected
                         (string-replace "Before *revised* prose 😀."
                                         "Before *revised* prose[[https://example.org][ ]]😀." expected))))
        (let ((repairs (gdocs-diff-generate native (gdocs-convert-org-string-to-ir expected))))
          ;; These pre-repair captures exposed a real inherited whitespace link.
          ;; Preserve that evidence instead of blessing it as a semantic no-op.
          (if (equal stage "created") (should-not repairs)
            (should repairs)
            (should (cl-every (lambda (request) (alist-get 'updateTextStyle request)) repairs))
            (let* ((span (seq-find (lambda (entry) (string-blank-p (plist-get entry :text)))
                                   (plist-get (car native) :native-link-spans)))
                   (update (alist-get 'updateTextStyle (car repairs))))
              (should span)
              (should (equal (alist-get 'fields update) "link"))
              (should (equal (alist-get 'range update)
                             (list (cons 'startIndex (plist-get span :start)) (cons 'endIndex (plist-get span :end))))))))))))

(ert-deftest gdocs-section-breaks-prose-edit-clears-inherited-links-before-authored-links ()
  "A native insertion's inherited link cannot leak onto unlinked surrounding text."
  (let* ((source "plain [[https://example.org][kept]].\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (text (alist-get 'text (alist-get 'insertText (car requests))))
         (seed `((updateTextStyle . ((textStyle . ((link . ((url . "https://inherited.invalid")))))
                                     (range . ((startIndex . 1) (endIndex . ,(gdocs-convert--string-to-utf16-length text))))
                                     (fields . "link")))))
         (native (gdocs-section-breaks-test--native (append (list (car requests) seed) (cdr requests)))))
    (should (equal (gdocs-convert-ir-to-org native) source))
    (should-not (gdocs-diff-generate native ir))))

(ert-deftest gdocs-section-breaks-word-edits-clear-hidden-whitespace-links ()
  "Word-level edits also clear inherited links invisible to normalized IR identity."
  (let* ((old (gdocs-convert-org-string-to-ir "old [[https://example.org][kept]].\n"))
         (new (gdocs-convert-org-string-to-ir "new [[https://example.org][kept]].\n"))
         (initial (append (gdocs-convert-ir-to-docs-requests old)
                          '(((updateTextStyle . ((textStyle . ((link . ((url . "https://inherited.invalid")))))
                                                 (range . ((startIndex . 4) (endIndex . 5))) (fields . "link")))))))
         (native (gdocs-section-breaks-test--native initial))
         (edits (gdocs-diff-generate native new))
         (result (gdocs-fidelity-test--returned-json (append initial edits))))
    (should edits)
    (seq-doseq (element (alist-get 'content (alist-get 'body result)))
      (seq-doseq (part (alist-get 'elements (alist-get 'paragraph element)))
        (when (equal (alist-get 'content (alist-get 'textRun part)) " ")
          (should-not (alist-get 'link (alist-get 'textStyle (alist-get 'textRun part)))))))
    (should (equal (gdocs-convert-ir-to-org (gdocs-section-breaks-test--native (append initial edits)))
                   "new [[https://example.org][kept]].\n"))))

(provide 'gdocs-section-breaks-test)
;;; gdocs-section-breaks-test.el ends here
