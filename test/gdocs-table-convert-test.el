;;; gdocs-table-convert-test.el --- Table conversion ownership -*- lexical-binding: t; -*-

;;; Commentary:
;; Table semantics and geometry compose shared inline conversion and neutral diff.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gdocs-convert)
(require 'gdocs-diff)

(defun gdocs-table-convert-test--paragraph (text &optional style)
  "Return a native paragraph containing TEXT and optional STYLE."
  (let* ((run (append (list (cons 'content (concat text "\n")))
                      (when style (list (cons 'textStyle style)))))
         (elements (list (list (cons 'textRun run))))
         (paragraph (list '(paragraphStyle . ((namedStyleType . "NORMAL_TEXT")))
                          (cons 'elements elements))))
    (list (cons 'paragraph paragraph))))

(defun gdocs-table-convert-test--cell (text &optional style)
  "Return a native cell containing TEXT and optional STYLE."
  (list (cons 'content (list (gdocs-table-convert-test--paragraph text style)))))

(ert-deftest gdocs-table-convert-bidirectional-rich-cells-and-edits ()
  "Rich Unicode cells and empty cells survive source/native/repeated conversion."
  (require 'gdocs-convert-tables)
  (let* ((source "| *Head 😀* | [[https://example.org][Link]] |\n|---+---|\n| alpha | |\n\nAfter 😀\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (header (list (gdocs-table-convert-test--cell "Head 😀" '((bold . t)))
                       (gdocs-table-convert-test--cell "Link" '((link . ((url . "https://example.org")))))))
         (body (list (gdocs-table-convert-test--cell "alpha") (gdocs-table-convert-test--cell "")))
         (rows (list (list (cons 'tableCells header)) (list (cons 'tableCells body))))
         (table (list '(rows . 2) '(columns . 2) (cons 'tableRows rows)))
         (table-element (list '(startIndex . 1) '(endIndex . 29) (cons 'table table)))
         (after (append '((startIndex . 29) (endIndex . 38))
                        (gdocs-table-convert-test--paragraph "After 😀")))
         (json (list (cons 'body (list (cons 'content (list table-element after))))))
         (remote (gdocs-convert-docs-json-to-ir json))
         (returned (gdocs-convert-ir-to-org remote))
         (again (gdocs-convert-org-string-to-ir returned)))
    (should (equal (gdocs-diff--element-keys ir) (gdocs-diff--element-keys remote)))
    (should (string-match-p (regexp-quote "*Head 😀*") returned))
    (should (string-match-p (regexp-quote "[[https://example.org][Link]]") returned))
    (should-not (gdocs-diff-generate ir again))
    (should-not (gdocs-diff-generate again (gdocs-convert-org-string-to-ir returned)))
    (should (gdocs-diff-generate remote (gdocs-convert-org-string-to-ir
                                         (string-replace "alpha" "beta" source))))))

(ert-deftest gdocs-table-convert-read-and-insert-geometry ()
  "Read geometry excludes the insertion-preserved paragraph and counts UTF-16."
  (require 'gdocs-convert-tables)
  (let* ((ir (car (gdocs-convert-org-string-to-ir
                   "| *Head 😀* | [[https://example.org][Link]] |\n|---+---|\n| alpha | |\n")))
         (plan (gdocs-convert--table-to-requests ir 5))
         (requests (plist-get plan :requests))
         (inserts (delq nil (mapcar (lambda (request) (alist-get 'insertText request)) requests))))
    (should (= (gdocs-diff--element-utf16-length ir) 28))
    (should (= (plist-get plan :index) 34))
    (should (equal (mapcar (lambda (insert) (alist-get 'index (alist-get 'location insert))) inserts)
                   '(14 11 9)))
    (should (equal (mapcar (lambda (insert) (alist-get 'text insert)) inserts) '("alpha" "Link" "Head 😀")))
    (should (seq-some (lambda (request)
                        (let ((style (alist-get 'updateTextStyle request)))
                          (and (eq (alist-get 'bold (alist-get 'textStyle style)) t)
                               (= (alist-get 'startIndex (alist-get 'range style)) 9)
                               (= (alist-get 'endIndex (alist-get 'range style)) 16)))) requests))
    (should (gdocs-convert-ir-to-docs-requests (list ir)))))

(ert-deftest gdocs-table-convert-physical-owners ()
  "Table conversion, diff descriptors and width repair belong to table modules."
  (dolist (entry '((gdocs-convert--table-to-ir . "gdocs-convert-tables")
                   (gdocs-convert--docs-table-to-ir . "gdocs-convert-tables")
                   (gdocs-convert--ir-table-to-org . "gdocs-convert-tables")
                   (gdocs-convert--table-to-requests . "gdocs-convert-tables")
                   (gdocs-convert--table-cell-requests . "gdocs-convert-tables")
                   (gdocs-sync--compute-table-width-fixup-requests . "gdocs-convert-tables")
                   (gdocs-diff--table-key . "gdocs-diff-tables")
                   (gdocs-diff--table-utf16-length . "gdocs-diff-tables")))
    (should (string-match-p (concat (cdr entry) "\\.elc?\\'") (symbol-file (car entry) 'defun)))))

(ert-deftest gdocs-table-convert-standalone-empty-plan ()
  "Table structural planning loads without facade, styling, transport or accounts."
  (let ((directory (file-name-directory (locate-library "gdocs-convert"))))
    (with-temp-buffer
      (let* ((program '(progn
                         (require 'gdocs-convert-tables)
                         (dolist (feature '(gdocs-convert gdocs-styles gdocs-style-resolve gdocs-sync gdocs-api gdocs-auth))
                           (when (featurep feature) (error "Table planning loaded %s" feature)))
                         (let ((plan (gdocs-convert--table-to-requests '(:type table :rows ((nil nil))) 4)))
                           (unless (and (= (plist-get plan :index) 12)
                                        (= (length (plist-get plan :requests)) 3))
                             (error "Standalone table geometry changed")))))
             (status (call-process (expand-file-name invocation-name invocation-directory) nil t nil
                                   "--batch" "-Q" "-L" directory "--eval" (prin1-to-string program))))
        (ert-info ((buffer-string)) (should (zerop status)))))))

(ert-deftest gdocs-table-convert-postfetch-width-plan-uses-native-indices ()
  "Width repair uses fetched table locations and skips mismatched column counts."
  (let* ((first (car (gdocs-convert-org-string-to-ir "| a | longer |\n")))
         (second (car (gdocs-convert-org-string-to-ir "| b |\n")))
         (json '((body . ((content . [((startIndex . 40) (table . ((columns . 2))))
                                      ((startIndex . 100) (table . ((columns . 2))))])))))
         (requests (gdocs-sync--compute-table-width-fixup-requests (list first second) json)))
    (should (gdocs-sync--ir-has-tables-p (list first second)))
    (should (= (length requests) 2))
    (dolist (request requests)
      (should (= (alist-get 'index
                            (alist-get 'tableStartLocation (alist-get 'updateTableColumnProperties request)))
                 40)))
    (should-not (gdocs-sync--compute-table-width-fixup-requests nil json))))

(ert-deftest gdocs-table-convert-native-cells-compose-paragraph-decoding ()
  "Native cell text is decoded once through the shared paragraph layer."
  (let ((decoder (symbol-function 'gdocs-convert-paragraph-docs-runs)) (calls 0))
    (cl-letf (((symbol-function 'gdocs-convert-paragraph-docs-runs)
               (lambda (&rest args) (setq calls (1+ calls)) (apply decoder args))))
      (let ((runs (gdocs-convert--docs-table-cell-to-runs
                   (gdocs-table-convert-test--cell "😀" '((bold . t))))))
        (should (equal (gdocs-convert--runs-to-plain-text runs) "😀"))
        (should (plist-get (car runs) :bold))
        (should (= calls 1))))))

(defun gdocs-table-convert-test--table-branch-p (tree)
  "Return non-nil when TREE contains quoted table-kind recognition."
  (and (consp tree)
       (or (equal tree ''table)
           (gdocs-table-convert-test--table-branch-p (car tree))
           (gdocs-table-convert-test--table-branch-p (cdr tree)))))

(ert-deftest gdocs-table-convert-diff-kernel-has-no-table-branches ()
  "The generic diff engine must consume descriptors instead of classifying tables."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "gdocs-diff.el"
                                            (file-name-directory (locate-library "gdocs-convert"))))
    (goto-char (point-min))
    (condition-case nil
        (while t (should-not (gdocs-table-convert-test--table-branch-p (read (current-buffer)))))
      (end-of-file nil))))

(ert-deftest gdocs-table-convert-retains-fail-closed-status-policy ()
  "Shared cell decoding still blocks unsupported semantic status labels."
  (should-error (gdocs-convert-org-string-to-ir "| [[status:green][Ship]] |\n") :type 'user-error))

(provide 'gdocs-table-convert-test)
;;; gdocs-table-convert-test.el ends here
