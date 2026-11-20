;;; gdocs-footnote-reference-seam-test.el --- Footnote reference seam tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for preserving Org footnote references as zero-width semantic runs and
;; exposing their exact Google Docs UTF-16 insertion indices during request
;; generation.

;;; Code:

(require 'ert)
(require 'seq)
(require 'gdocs-convert)
(require 'gdocs-diff)
(require 'gdocs-sync)
(require 'json)

(defconst gdocs-footnote-test--directory (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the footnote regression fixtures.")

(ert-deftest gdocs-footnotes-label-filter-preserves-unrelated-annotations ()
  "Consuming native label metadata leaves unrelated paragraph annotations intact."
  (let ((markers '((:type footnote-label :data "authored-name") (:type custom :data "keep"))))
    (should (equal (gdocs-convert-footnote-paragraph-markers markers) '((:type custom :data "keep"))))))

(ert-deftest gdocs-footnotes-native-label-stays-reference-owned-on-note-edit ()
  "A real native label cannot make a note-only edit replace its host paragraph."
  (let* ((json-object-type 'alist) (json-array-type 'vector)
         (document (json-read-file (expand-file-name "fixtures/live-native-footnote-create.json" gdocs-footnote-test--directory)))
         (native (gdocs-sync--filter-empty-paragraphs
                  (gdocs-sync--filter-title (gdocs-convert-docs-json-to-ir document))))
         (source (copy-tree native))
         (note (seq-find (lambda (element) (eq (plist-get element :type) 'footnote)) native))
         (gdocs-convert-footnote-reference-text-function (lambda (_reference) "⟦note⟧")))
    ;; Source IR has a reference label, not its remote identification annotation.
    (dolist (element source)
      (when (eq (plist-get element :type) 'paragraph)
        (plist-put element :gdocs-marker nil)
        (dolist (run (plist-get element :contents))
          (when (plist-get run :footnote-label) (plist-put run :text "⟦note⟧")))))
    (should-not (gdocs-diff-generate native source))
    (let* ((updated-note (seq-find (lambda (element) (eq (plist-get element :type) 'footnote)) source))
           (definition (seq-find (lambda (element) (eq (plist-get element :type) 'footnote))
                                 (gdocs-convert-org-string-to-ir
                                  "[fn:authored-name] 😀 bold and *revised emphasis* with [[https://example.org/review][a revised link]].\n"))))
      (plist-put updated-note :contents (plist-get definition :contents))
      (let ((requests (gdocs-diff-generate native source)))
        (should requests)
        (dolist (request requests)
          (let* ((operation (cdar request))
                 (target (or (alist-get 'range operation) (alist-get 'location operation))))
            (should (equal (alist-get 'segmentId target) (plist-get note :segment-id)))))))))

(ert-deftest gdocs-footnotes-live-create-edit-preserves-label-richness-and-segment ()
  "Captured create and native edit data retain authored semantics and identity."
  (let (first-body first-segment)
    (dolist (case '(("live-native-footnote-create.json" . "[fn:authored-name] 😀 *bold* and /italic/ with [[https://example.org/notes][a link]].\n")
                    ("live-native-footnote-remote-edit.json" . "[fn:authored-name] 😀 bold and *revised remote-emphasis* with [[https://example.org/review][a revised link]].\n")))
      (let* ((json-object-type 'alist) (json-array-type 'vector)
             (document (json-read-file (expand-file-name (concat "fixtures/" (car case)) gdocs-footnote-test--directory)))
             (native (gdocs-sync--filter-empty-paragraphs (gdocs-sync--filter-title (gdocs-convert-docs-json-to-ir document))))
             (notes (seq-filter (lambda (element) (eq (plist-get element :type) 'footnote)) native))
             (expected (seq-find (lambda (element) (eq (plist-get element :type) 'footnote))
                                 (gdocs-convert-org-string-to-ir (cdr case)))))
        (should-not (alist-get 'revisionId document))
        (should (= (length notes) 1))
        (should (equal (plist-get (car notes) :label) "authored-name"))
        (should (equal (gdocs-diff-footnotes--key (car notes)) (gdocs-diff-footnotes--key expected)))
        (dolist (element native)
          (should-not (seq-some (lambda (marker) (eq (plist-get marker :type) 'footnote-label))
                                (plist-get element :gdocs-marker))))
        (if first-segment
            (progn (should (equal first-body (alist-get 'body document)))
                   (should (equal first-segment (plist-get (car notes) :segment-id))))
          (setq first-body (alist-get 'body document) first-segment (plist-get (car notes) :segment-id)))))))

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
