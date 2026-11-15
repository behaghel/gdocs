;;; gdocs-link-resolve-test.el --- Document-link lookup boundaries -*- lexical-binding: t; -*-

;;; Commentary:
;; Only synthetic, temporary notes and in-memory document JSON are used.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-id)
(require 'gdocs-convert)
(require 'gdocs-diff)

(defun gdocs-link-resolve-test--text (text &optional style)
  "Return a native text element for TEXT with optional STYLE."
  (list (cons 'textRun (append (list (cons 'content text))
                               (when style (list (cons 'textStyle style)))))))

(defun gdocs-link-resolve-test--heading (text id)
  "Return a native heading paragraph for TEXT and ID."
  (list (cons 'paragraph
              (list (cons 'paragraphStyle (list '(namedStyleType . "HEADING_1") (cons 'headingId id)))
                    (cons 'elements (list (gdocs-link-resolve-test--text (concat text "\n"))))))))

(defmacro gdocs-link-resolve-test--with-notes (&rest body)
  "Run BODY with temporary linked source/target notes and private fixture caches."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "gdocs-link-fixtures-" t))
          (source-file (expand-file-name "source.org" directory))
          (target-file (expand-file-name "target.org" directory))
          (plain-file (expand-file-name "plain.org" directory))
          (source-text (concat ":PROPERTIES:\n:GDOCS_DOCUMENT_ID: fixture-source\n:END:\n"
                               "*[[file:target.org::*Target Heading][😀 label]]* "
                               "[[*Local Heading][self]] [[file:plain.org][plain]] "
                               "[[https://example.org][web]].\n\n"
                               "* Local Heading\n:PROPERTIES:\n:ID: fixture-local-id\n:END:\n"))
          (gdocs-convert--heading-cache (make-hash-table :test 'equal))
          (gdocs-convert--heading-reverse-cache (make-hash-table :test 'equal))
          (org-id-locations (make-hash-table :test 'equal)))
     (unwind-protect
         (progn
           (with-temp-file source-file (insert source-text))
           (with-temp-file target-file
             (insert ":PROPERTIES:\n:GDOCS_DOCUMENT_ID: fixture-target\n:END:\n* Target Heading\n"))
           (with-temp-file plain-file (insert "Unpublished note\n"))
           (puthash "fixture-local-id" source-file org-id-locations)
           ,@body)
       (delete-directory directory t))))

(ert-deftest gdocs-link-resolve-full-local-pipeline ()
  "File, heading, ID and unknown links preserve rich labels through native import."
  (require 'gdocs-link-resolve)
  (gdocs-link-resolve-test--with-notes
   (let* ((gdocs-convert--link-context
           (list :buffer-file source-file :docid-map (gdocs-convert--build-docid-to-file-map (list directory))))
          (gdocs-convert--document-id "fixture-source")
          (target-url "https://docs.google.com/document/d/fixture-target/edit#heading=h.target-anchor")
          (local-url "https://docs.google.com/document/d/fixture-source/edit#heading=h.local-anchor"))
     (gdocs-convert--cache-heading-ids "fixture-target"
                                       (list (cons 'body (list (cons 'content
                                                                     (list (gdocs-link-resolve-test--heading "Target Heading" "target-anchor")))))))
     (gdocs-convert--cache-heading-ids "fixture-source"
                                       (list (cons 'body (list (cons 'content
                                                                     (list (gdocs-link-resolve-test--heading "Local Heading" "local-anchor")))))))
     (cl-letf (((symbol-function 'org-id-locations-load)
                (lambda (&rest _) (ert-fail "Unexpected access to an ID database"))))
       (let* ((ir (gdocs-convert-org-string-to-ir source-text))
              (parts (list (gdocs-link-resolve-test--text "😀 label" `((bold . t) (link . ((url . ,target-url)))))
                           (gdocs-link-resolve-test--text " ")
                           (gdocs-link-resolve-test--text "self" '((link . ((headingId . "local-anchor")))))
                           (gdocs-link-resolve-test--text " plain ")
                           (gdocs-link-resolve-test--text "web" '((link . ((url . "https://example.org")))))
                           (gdocs-link-resolve-test--text ".\n")))
              (body (list (cons 'startIndex 1) (cons 'endIndex 26)
                          (cons 'paragraph (list (cons 'elements parts)
                                                 '(paragraphStyle . ((namedStyleType . "NORMAL_TEXT")))))))
              (json (list (cons 'body (list (cons 'content
                                                  (list body (gdocs-link-resolve-test--heading "Local Heading" "local-anchor")))))))
              (remote (gdocs-convert-docs-json-to-ir json))
              (returned (gdocs-convert-ir-to-org remote))
              (again (gdocs-convert-org-string-to-ir returned))
              (requests (gdocs-convert-ir-to-docs-requests ir)))
         (should (equal (gdocs-convert--resolve-id-link "fixture-local-id") local-url))
         (should (equal (gdocs-diff--element-keys ir) (gdocs-diff--element-keys remote)))
         (should (string-match-p (regexp-quote "*[[file:target.org::*Target Heading][😀 label]]*") returned))
         (should (string-match-p (regexp-quote "[[id:fixture-local-id][self]]") returned))
         (should-not (gdocs-diff-generate ir again))
         (should-not (gdocs-diff-generate again (gdocs-convert-org-string-to-ir returned)))
         (should (seq-some (lambda (request)
                             (equal (alist-get 'url (alist-get 'link
                                                               (alist-get 'textStyle (alist-get 'updateTextStyle request))))
                                    target-url)) requests))
         (should (equal (gdocs-convert--reverse-resolve-link
                         "https://docs.google.com/document/d/unknown-fixture/edit")
                        "https://docs.google.com/document/d/unknown-fixture/edit")))))))

(ert-deftest gdocs-link-resolve-owns-lookup-and-cache-definitions ()
  "Conversion cannot retain lookup helpers or link state as hidden owners."
  (dolist (function '(gdocs-convert--read-file-property-gdocs-id gdocs-convert--build-docid-to-file-map
                                                                 gdocs-convert--cache-heading-ids gdocs-convert--link-url gdocs-convert--resolve-id-link
                                                                 gdocs-convert--resolve-file-link gdocs-convert--inline-link-for-org
                                                                 gdocs-convert--reverse-resolve-link gdocs-convert--heading-id-to-url))
    (should (string-match-p "gdocs-link-resolve\\.elc?\\'" (symbol-file function 'defun)))))

(ert-deftest gdocs-link-resolve-metadata-lookups-do-not-run-org-hooks ()
  "Temporary heading reads are data operations, not user editing sessions."
  (gdocs-link-resolve-test--with-notes
   (let ((org-mode-hook (list (lambda () (ert-fail "Lookup ran an Org user hook")))))
     (should (equal (gdocs-convert--heading-at-pos
                     source-file (with-temp-buffer (insert source-text) (goto-char (point-min))
                                                   (search-forward "* Local Heading") (line-beginning-position)))
                    "Local Heading"))
     (should (equal (gdocs-convert--find-heading-org-id source-file "Local Heading") "fixture-local-id")))))

(ert-deftest gdocs-link-resolve-loads-without-conversion-or-transport ()
  "Document-link services load standalone without styles, conversion or accounts."
  (let ((directory (file-name-directory (locate-library "gdocs-convert"))))
    (with-temp-buffer
      (let* ((program '(progn
                         (require 'gdocs-link-resolve)
                         (dolist (feature '(gdocs-convert gdocs-style-resolve gdocs-styles gdocs-diff gdocs-api gdocs-auth))
                           (when (featurep feature) (error "Link lookup loaded %s" feature)))
                         (let ((gdocs-convert--document-id "fixture-doc"))
                           (unless (equal (gdocs-convert--heading-id-to-url "anchor")
                                          "https://docs.google.com/document/d/fixture-doc/edit#heading=h.anchor")
                             (error "Standalone native anchor resolution changed")))))
             (status (call-process (expand-file-name invocation-name invocation-directory) nil t nil
                                   "--batch" "-Q" "-L" directory "--eval" (prin1-to-string program))))
        (ert-info ((buffer-string)) (should (zerop status)))))))

(ert-deftest gdocs-link-resolve-id-lookup-does-not-run-org-hooks ()
  "Org-ID's own temporary buffer must not become a user editing session."
  (gdocs-link-resolve-test--with-notes
   (let ((scratch (generate-new-buffer " *gdocs-ID-fixture*"))
         (org-mode-hook (list (lambda () (ert-fail "ID lookup ran an Org user hook")))))
     (unwind-protect
         (cl-letf (((symbol-function 'org-get-buffer-create) (lambda (&rest _) scratch))
                   ((symbol-function 'org-id-locations-load)
                    (lambda (&rest _) (ert-fail "Unexpected access to an ID database"))))
           (should (equal (gdocs-convert--resolve-id-link "fixture-local-id")
                          "https://docs.google.com/document/d/fixture-source/edit")))
       (kill-buffer scratch)))))

(defun gdocs-link-resolve-test--contains (tree symbol)
  "Return non-nil when TREE contains SYMBOL."
  (if (consp tree)
      (or (gdocs-link-resolve-test--contains (car tree) symbol)
          (gdocs-link-resolve-test--contains (cdr tree) symbol))
    (eq tree symbol)))

(ert-deftest gdocs-link-resolve-facade-has-no-filesystem-or-cache-policy ()
  "Conversion must not retain file reads, ID lookup or cache state behind helpers."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "gdocs-convert.el"
                                            (file-name-directory (locate-library "gdocs-convert"))))
    (goto-char (point-min))
    (let (forms)
      (condition-case nil (while t (push (read (current-buffer)) forms)) (end-of-file nil))
      (dolist (symbol '(insert-file-contents directory-files org-id-find file-readable-p
                                             gdocs-convert--link-context gdocs-convert--heading-cache
                                             gdocs-convert--heading-reverse-cache gdocs-convert--docs-url-regexp))
        (should-not (gdocs-link-resolve-test--contains forms symbol))))))

(ert-deftest gdocs-link-resolve-missing-id-cannot-trigger-a-rescan ()
  "A cached-ID miss returns nil without database loading or recursive discovery."
  (let ((org-id-locations (make-hash-table :test 'equal)))
    (with-temp-buffer
      (cl-letf (((symbol-function 'org-id-locations-load)
                 (lambda (&rest _) (ert-fail "Unexpected access to an ID database")))
                ((symbol-function 'directory-files-recursively)
                 (lambda (&rest _) (ert-fail "Unexpected recursive file discovery")))
                ((symbol-function 'org-id-update-id-locations)
                 (lambda (&rest _) (ert-fail "Unexpected broad ID-location update"))))
        (should-not (gdocs-convert--resolve-id-link "missing-fixture-id"))))))

(ert-deftest gdocs-link-resolve-explicit-directory-scan-is-bounded ()
  "Only immediate Org files in explicit directories contribute document IDs."
  (gdocs-link-resolve-test--with-notes
   (let* ((nested (expand-file-name "nested" directory))
          (ignored (expand-file-name "ignored.txt" directory)))
     (make-directory nested)
     (with-temp-file (expand-file-name "nested.org" nested)
       (insert ":PROPERTIES:\n:GDOCS_DOCUMENT_ID: nested-fixture\n:END:\n"))
     (with-temp-file ignored (insert ":PROPERTIES:\n:GDOCS_DOCUMENT_ID: ignored-fixture\n:END:\n"))
     (let ((map (gdocs-convert--build-docid-to-file-map (list directory))))
       (should (= (hash-table-count map) 2))
       (should (equal (gethash "fixture-source" map) source-file))
       (should (equal (gethash "fixture-target" map) target-file))
       (should-not (gethash "nested-fixture" map))
       (should-not (gethash "ignored-fixture" map))))))

(provide 'gdocs-link-resolve-test)
;;; gdocs-link-resolve-test.el ends here
