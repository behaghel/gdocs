;;; gdocs-footnote-reference-seam-test.el --- Footnote reference seam tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for preserving Org footnote references as zero-width semantic runs and
;; exposing their exact Google Docs UTF-16 insertion indices during request
;; generation.

;;; Code:

(require 'ert)
(require 'seq)
(require 'gdocs-convert)

(ert-deftest gdocs-convert-org-footnote-reference-becomes-semantic-run ()
  "Org footnote references are not converted to literal [fn:] text."
  (let* ((ir (gdocs-convert-org-string-to-ir "Text[fn:one].\n\n* Footnotes\n\n[fn:one] Body.\n"))
         (paragraph (car ir))
         (runs (plist-get paragraph :contents)))
    (should (equal "Text." (gdocs-convert--runs-to-plain-text runs)))
    (should (seq-find (lambda (run)
                        (equal "one" (plist-get run :footnote-label)))
                      runs))))

(ert-deftest gdocs-convert-footnote-reference-handler-receives-doc-index ()
  "Request generation reports exact footnote reference insertion indices."
  (let* ((ir (gdocs-convert-org-string-to-ir "Text[fn:one].\n"))
         (seen nil)
         (gdocs-convert-footnote-reference-handler
          (lambda (run index)
            (push (list :label (plist-get run :footnote-label)
                        :index index)
                  seen)
            nil))
         (requests (gdocs-convert-ir-to-docs-requests ir 1)))
    (should (equal (nreverse seen) '((:label "one" :index 5))))
    (should (equal (alist-get 'text (alist-get 'insertText (car requests)))
                   "Text.\n"))))

(provide 'gdocs-footnote-reference-seam-test)
;;; gdocs-footnote-reference-seam-test.el ends here
