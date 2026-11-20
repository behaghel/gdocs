;;; gdocs-text-paragraphs-test.el --- Composable text foundation -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise standalone primitives and their use by the real publishing pipeline.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gdocs-convert)
(require 'gdocs-diff)

(ert-deftest gdocs-text-paragraphs-independent-data-modules ()
  "Text and paragraph primitives load without the conversion facade or policy."
  (let ((directory (file-name-directory (locate-library "gdocs-convert"))))
    (with-temp-buffer
      (let* ((program
              '(progn
                 (require 'gdocs-convert-text)
                 (require 'gdocs-convert-paragraphs)
                 (dolist (feature '(gdocs-convert gdocs-styles gdocs-api gdocs-auth
                                                  gdocs-convert-footnotes gdocs-convert-status
                                                  gdocs-convert-headings org))
                   (when (featurep feature) (error "Primitive loaded %s" feature)))
                 (let* ((run (gdocs-convert-make-run "😀" :bold t))
                        (paragraph (gdocs-convert-make-paragraph (list run)))
                        (plan (gdocs-convert-paragraph-text-requests
                               (gdocs-convert--runs-to-plain-text
                                (plist-get paragraph :contents)) 4)))
                   (unless (and (eq (plist-get paragraph :style) 'normal)
                                (equal (plist-get plan :length) 3)
                                (equal (plist-get plan :requests)
                                       '(((insertText . ((text . "😀\n")
                                                         (location . ((index . 4)))))))))
                     (error "Standalone paragraph composition failed")))))
             (status (call-process
                      (expand-file-name invocation-name invocation-directory) nil t nil
                      "--batch" "-Q" "-L" directory "--eval" (prin1-to-string program))))
        (ert-info ((buffer-string)) (should (zerop status)))))))

(ert-deftest gdocs-text-paragraphs-owner-boundaries ()
  "The facade uses primitive definitions rather than retaining duplicate owners."
  (dolist (function '(gdocs-convert-make-run gdocs-convert--make-run
                                             gdocs-convert--make-plain-run gdocs-convert--string-to-run
                                             gdocs-convert--runs-to-plain-text gdocs-convert--strip-vertical-tabs
                                             gdocs-convert--string-to-utf16-length gdocs-convert--make-insert-text-request))
    (should (string-match-p "gdocs-convert-text\\.elc?\\'" (symbol-file function 'defun))))
  (should (string-match-p "gdocs-convert-paragraphs\\.elc?\\'"
                          (symbol-file 'gdocs-convert-make-paragraph 'defun))))

(ert-deftest gdocs-text-paragraphs-plain-plan-boundaries ()
  "Paragraph insertion preserves literal text, prefixes, empty bodies and UTF-16."
  (dolist (case '(("" "" 1 "\n" 1)
                  ("😀 é" "\t" 7 "\t😀 é\n" 7)
                  ("a\nb\t" "" 3 "a\nb\t\n" 5)))
    (pcase-let ((`(,text ,prefix ,index ,full ,length) case))
      (should (equal (gdocs-convert-paragraph-text-requests text index prefix)
                     (list :requests
                           (list `((insertText . ((text . ,full)
                                                  (location . ((index . ,index)))))))
                           :length length))))))

(ert-deftest gdocs-text-paragraphs-source-versus-literal-text ()
  "Only source soft wraps are normalized; literal text and inherited data survive."
  (let* ((props (list :bold t :link "https://example.org"))
         (before (copy-tree props))
         (run (car (gdocs-convert--string-to-run
                    (propertize "one\ntwo" 'face 'bold) props))))
    (should (equal props before))
    (should (equal (plist-get run :text) "one two"))
    (should-not (text-properties-at 0 (plist-get run :text)))
    (should (plist-get run :bold))
    (should (equal (plist-get run :link) "https://example.org"))
    (should (equal (plist-get (gdocs-convert-make-run "one\ntwo") :text) "one\ntwo"))
    (should (equal (gdocs-convert--strip-vertical-tabs "one\vtwo") "one\ntwo"))
    (should-not (gdocs-convert--string-to-run "" props)))
  (should-error (gdocs-convert-paragraph-text-requests '((:text "unresolved")) 1)
                :type 'wrong-type-argument))

(ert-deftest gdocs-text-paragraphs-publishing-composes-the-primitive ()
  "Real heading, list, block and body publishing uses shared paragraph insertion."
  (let ((original (symbol-function 'gdocs-convert-paragraph-text-requests)) calls)
    (cl-letf (((symbol-function 'gdocs-convert-paragraph-text-requests)
               (lambda (text index &optional prefix)
                 (push text calls)
                 (funcall original text index prefix))))
      (gdocs-convert-ir-to-docs-requests
       (gdocs-convert-org-string-to-ir
        "* Heading\n\nBody 😀\nsoft wrap.\n\n- List item\n\n#+begin_quote\nQuoted text\n#+end_quote\n")))
    (dolist (text '("Heading" "Body 😀 soft wrap." "List item" "Quoted text"))
      (should (member text calls)))))

(ert-deftest gdocs-text-paragraphs-bidirectional-edit-and-repeat ()
  "Native Docs text and source wrapping share a stable paragraph model."
  (let* ((source "Alpha 😀\nsoft wrap.\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (text "Alpha 😀 soft wrap.\n")
         (json `((body . ((content . [((startIndex . 1) (endIndex . 21)
                                       (paragraph . ((elements . [((textRun . ((content . ,text))))])
                                                     (paragraphStyle . ((namedStyleType . "NORMAL_TEXT"))))))])))))
         (remote (gdocs-convert-docs-json-to-ir json))
         (returned (gdocs-convert-ir-to-org remote))
         (again (gdocs-convert-org-string-to-ir returned)))
    (should (equal (gdocs-diff--element-keys ir) (gdocs-diff--element-keys remote)))
    (should-not (gdocs-diff-generate ir again))
    (should-not (gdocs-diff-generate again (gdocs-convert-org-string-to-ir returned)))
    (let* ((edited (gdocs-convert-org-string-to-ir "Alpha 😀 edited.\n"))
           (requests (gdocs-diff-generate remote edited)))
      (should (seq-some (lambda (request) (alist-get 'insertText request)) requests))
      (should-not (gdocs-diff-generate edited edited)))))

(provide 'gdocs-text-paragraphs-test)
;;; gdocs-text-paragraphs-test.el ends here
