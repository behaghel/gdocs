;;; gdocs-disposable-plan.el --- Offline plans for disposable live checks -*- lexical-binding: t; -*-

;;; Commentary:
;; This helper does no authentication or HTTP.  It writes plans and evaluates
;; fetched disposable JSON using the source checkout selected on load-path.

;;; Code:

(require 'gdocs-convert)
(require 'gdocs-diff)
(require 'json)
(require 'seq)

(defun gdocs-disposable-plan--read-source (file)
  "Read synthetic Org source FILE."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun gdocs-disposable-plan--write-json (file value)
  "Write nonsensitive fixture VALUE to artifact FILE."
  (with-temp-file file (insert (json-encode value)) (insert "\n")))

(defun gdocs-disposable-plan--body (ir)
  "Remove metadata title and the final structural empty paragraph from IR."
  (setq ir (seq-remove (lambda (element) (eq (plist-get element :source) 'metadata)) ir))
  (let ((last (car (last ir))))
    (when (and (eq (plist-get last :type) 'paragraph)
               (eq (plist-get last :style) 'normal)
               (not (plist-get last :list)) (not (plist-get last :list-continuation))
               (not (plist-get last :gdocs-marker))
               (string-empty-p (gdocs-convert--runs-to-plain-text (plist-get last :contents))))
      (setq ir (butlast ir))))
  ir)

(defun gdocs-disposable-plan-create (source directory)
  "Write a create plan for synthetic SOURCE in artifact DIRECTORY."
  (let* ((ir (gdocs-convert-org-string-to-ir (gdocs-disposable-plan--read-source source)))
         (requests (gdocs-convert-ir-to-docs-requests ir)))
    (gdocs-disposable-plan--write-json (expand-file-name "create.requests.json" directory)
                                       (list (cons 'requests (vconcat requests))))
    (with-temp-file (expand-file-name "source-canonical.org" directory)
      (insert (gdocs-convert-ir-to-org ir)))
    (list :elements (length ir) :requests (length requests))))

(defun gdocs-disposable-plan-inspect (source remote directory)
  "Compare synthetic SOURCE with fetched REMOTE JSON and save plans in DIRECTORY."
  (let* ((json-object-type 'alist) (json-array-type 'vector) (json-key-type 'symbol)
         (json (json-read-file remote))
         (local (gdocs-convert-org-string-to-ir (gdocs-disposable-plan--read-source source)))
         (native (gdocs-disposable-plan--body (gdocs-convert-docs-json-to-ir json)))
         (requests (gdocs-diff-generate native local))
         (revision (alist-get 'revisionId json))
         (body (list (cons 'requests (vconcat requests)))))
    (when revision (setq body (append body (list (cons 'writeControl (list (cons 'requiredRevisionId revision)))))))
    (gdocs-disposable-plan--write-json (expand-file-name "diff.requests.json" directory) body)
    (with-temp-file (expand-file-name "returned.org" directory) (insert (gdocs-convert-ir-to-org native)))
    (with-temp-file (expand-file-name "returned-ir.el" directory) (prin1 native (current-buffer)))
    (list :local-elements (length local) :remote-elements (length native) :diff-requests (length requests)
          :request-kinds (delete-dups (mapcar #'caar requests)))))

(defun gdocs-disposable-plan-remote-edit (remote source directory)
  "Check native numbering in REMOTE and plan edits of synthetic SOURCE in DIRECTORY."
  (let* ((json-object-type 'alist) (json-array-type 'vector) (json-key-type 'symbol)
         (json (json-read-file remote)) parent-one parent-two continuation task label)
    (seq-doseq (element (alist-get 'content (alist-get 'body json)))
      (when-let* ((paragraph (alist-get 'paragraph element)))
        (let ((text (mapconcat (lambda (run) (or (alist-get 'content (alist-get 'textRun run)) ""))
                               (alist-get 'elements paragraph) "")))
          (cond ((equal text "Parent one\n") (setq parent-one (alist-get 'bullet paragraph)))
                ((equal text "Parent two\n") (setq parent-two (alist-get 'bullet paragraph)))
                ((string-prefix-p "Parent continuation" text) (setq continuation paragraph))
                ((equal text "☐ Open task\n") (setq task (alist-get 'startIndex element)))
                ((and (null label) (string-match (regexp-quote "Audit 😀") text))
                 (setq label (+ (alist-get 'startIndex element)
                                (gdocs-convert--string-to-utf16-length (substring text 0 (match-beginning 0))))))))))
    (unless (and parent-one parent-two continuation task label
                 (equal (alist-get 'listId parent-one) (alist-get 'listId parent-two))
                 (= (or (alist-get 'nestingLevel parent-one) 0) 0)
                 (= (or (alist-get 'nestingLevel parent-two) 0) 0)
                 (not (alist-get 'bullet continuation)))
      (error "Native parent list identity, nesting or continuation bullet invariant failed"))
    (let* ((id (alist-get 'listId parent-one))
           (levels (alist-get 'nestingLevels (alist-get 'listProperties (alist-get (intern id) (alist-get 'lists json)))))
           (definition (elt levels 0))
           (requests (list `((deleteContentRange . ((range . ((startIndex . ,label) (endIndex . ,(+ label (gdocs-convert--string-to-utf16-length "Audit 😀"))))))))
                           `((insertText . ((text . "Remote title 😀") (location . ((index . ,label))))))
                           `((deleteContentRange . ((range . ((startIndex . ,task) (endIndex . ,(1+ task)))))))
                           `((insertText . ((text . "☑") (location . ((index . ,task))))))
                           '((replaceAllText . ((containsText . ((text . "Open task") (matchCase . t))) (replaceText . "Remote edited open task"))))))
           (body (list (cons 'requests (vconcat requests))))
           (expected (string-replace "- [ ] Open task" "- [X] Remote edited open task" (gdocs-disposable-plan--read-source source))))
      (unless (and (equal (alist-get 'glyphType definition) "DECIMAL") (= (or (alist-get 'startNumber definition) 1) 1))
        (error "Native parent list is not a decimal list starting at one"))
      (when-let* ((revision (alist-get 'revisionId json)))
        (setq body (append body (list (cons 'writeControl (list (cons 'requiredRevisionId revision)))))))
      (gdocs-disposable-plan--write-json (expand-file-name "remote-edit.requests.json" directory) body)
      (let ((position (string-match (regexp-quote ":title \"Audit 😀\"") expected)))
        (unless position (error "Synthetic first callout title is missing"))
        (setq expected (concat (substring expected 0 position) ":title \"Remote title 😀\""
                               (substring expected (+ position (length ":title \"Audit 😀\""))))))
      (with-temp-file (expand-file-name "expected-after-remote.org" directory) (insert expected))
      (list :native-parent-list-id-shared t :native-parent-level 0 :native-numbering 'decimal-from-one
            :continuation-bullet nil :remote-edit-requests (length requests)))))

(defun gdocs-disposable-plan-local-edit (source directory)
  "Write controlled local edits of synthetic SOURCE in DIRECTORY."
  (let ((text (gdocs-disposable-plan--read-source source)))
    (dolist (change '(("   2. Child two" . "      1. Child two *updated*")
                      ("- [-] Partial task" . "- [X] Partial task")
                      ("src_python{  x + 1  }" . "src_python{  x + 2  }")
                      ("- [ ] Callout task" . "- [-] Callout task")
                      ("*Term 😀*" . "*Updated term 😀*")))
      (unless (string-match-p (regexp-quote (car change)) text) (error "Synthetic edit target is missing"))
      (setq text (string-replace (car change) (cdr change) text)))
    (with-temp-file (expand-file-name "local-updated.org" directory) (insert text))
    (list :controlled-local-edits 5)))

(defun gdocs-disposable-plan-export-fixture (remote destination)
  "Export disposable REMOTE JSON to new fixture DESTINATION without remote identity."
  (when (file-exists-p destination) (error "Fixture destination already exists"))
  (let* ((json-object-type 'alist) (json-array-type 'vector) (json-key-type 'symbol)
         (json (json-read-file remote)))
    (setf (alist-get 'documentId json) "disposable-fixture"
          (alist-get 'title json) "Disposable native list fixture")
    (setq json (assq-delete-all 'revisionId json))
    (gdocs-disposable-plan--write-json destination json)
    (list :fixture-written t :revision-removed t)))

(defun gdocs-disposable-plan-repeat-edit (source directory)
  "Write a second synthetic SOURCE update into DIRECTORY."
  (let ((text (gdocs-disposable-plan--read-source source)))
    (dolist (change '(("This document contains synthetic test content only." . "Only synthetic content 😀 is present.")
                      ("      1. Child two *updated*" . "   2. Child two *updated again*")
                      ("- [X] Remote edited open task" . "- [-] Remote edited open task")
                      ("src_python{  x + 2  }" . "src_python{  x + 3  }")
                      (":title \"Remote title 😀\"" . ":title \"Second update 😀\"")))
      (unless (string-match-p (regexp-quote (car change)) text) (error "Synthetic second-update target is missing"))
      (setq text (string-replace (car change) (cdr change) text)))
    (with-temp-file (expand-file-name "second-update.org" directory) (insert text))
    (list :controlled-second-update-edits 5)))

(defun gdocs-disposable-plan-style-repair (source remote directory)
  "Plan final callout chrome for identical SOURCE and REMOTE in DIRECTORY."
  (let* ((json-object-type 'alist) (json-array-type 'vector) (json-key-type 'symbol)
         (json (json-read-file remote))
         (local (gdocs-convert-org-string-to-ir (gdocs-disposable-plan--read-source source)))
         (native (gdocs-disposable-plan--body (gdocs-convert-docs-json-to-ir json)))
         (revision (alist-get 'revisionId json)) requests)
    (unless (and (stringp revision) (not (gdocs-diff-generate native local)))
      (user-error "Style-only repair requires identical semantic source and a revision"))
    (dolist (range (gdocs-diff-lists--final-ranges (gdocs-diff-semantic-project-ir local) 1))
      (when (eq (plist-get (plist-get range :element) :type) 'callout-block)
        (setq requests
              (append requests
                      (seq-filter (lambda (request)
                                    ;; Reapplying named styles can clear authored
                                    ;; emphasis in Docs.  Only final list chrome
                                    ;; is safe for this diagnostic repair.
                                    (equal (alist-get 'fields (alist-get 'updateParagraphStyle request))
                                           "indentStart,indentFirstLine,borderLeft"))
                                  (plist-get (gdocs-convert--callout-block-to-requests
                                              (plist-get range :element) (plist-get range :start)) :requests))))))
    (gdocs-disposable-plan--write-json
     (expand-file-name "style-repair.requests.json" directory)
     (list (cons 'requests (vconcat requests))
           (cons 'writeControl (list (cons 'requiredRevisionId revision)))))
    (list :paragraph-style-requests (length requests) :content-mutations 0)))

(provide 'gdocs-disposable-plan)
;;; gdocs-disposable-plan.el ends here
