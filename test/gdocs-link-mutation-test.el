;;; gdocs-link-mutation-test.el --- Native link mutation fidelity -*- lexical-binding: t; -*-

;;; Commentary:
;; Native link addresses survive semantic whitespace normalization as metadata.
;; Clearing exact native spans must precede text mutation and authored restoration.

;;; Code:

(require 'ert)
(require 'gdocs-fidelity-corrections-test)
(require 'gdocs-sync)

(defconst gdocs-link-mutation-test--directory (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing native link mutation fixtures.")

(defun gdocs-link-mutation-test--live (stage)
  "Read sanitized actual body response at STAGE."
  (let* ((json-object-type 'alist) (json-array-type 'vector)
         (document (json-read-file (expand-file-name (format "fixtures/live-link-mutation-%s.json" stage)
                                                     gdocs-link-mutation-test--directory))))
    (should-not (alist-get 'revisionId document))
    (gdocs-sync--filter-empty-paragraphs (gdocs-sync--filter-title (gdocs-convert-docs-json-to-ir document)))))

(defun gdocs-link-mutation-test--before-paragraph (ir)
  "Find the synthetic Before paragraph in IR."
  (seq-find (lambda (element) (string-prefix-p "Before " (gdocs-convert--runs-to-plain-text
                                                          (plist-get element :contents)))) ir))

(defun gdocs-link-mutation-test--dirty-requests ()
  "Return synthetic prose with an unintended hyperlink on one space."
  (append (gdocs-convert-ir-to-docs-requests
           (gdocs-convert-org-string-to-ir "old [[https://example.org][kept]].\n"))
          '(((updateTextStyle . ((textStyle . ((link . ((url . "https://inherited.invalid")))))
                                 (range . ((startIndex . 4) (endIndex . 5))) (fields . "link")))))))

(defun gdocs-link-mutation-test--native (requests)
  "Decode REQUESTS with normal sync filtering."
  (gdocs-sync--filter-empty-paragraphs
   (gdocs-sync--filter-title
    (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json requests)))))

(ert-deftest gdocs-link-mutation-noop-repairs-only-an-unintended-whitespace-link ()
  "An otherwise unchanged paragraph clears the exact hidden link, then converges."
  (let* ((initial (gdocs-link-mutation-test--dirty-requests))
         (old (gdocs-link-mutation-test--native initial))
         (new (gdocs-convert-org-string-to-ir "old [[https://example.org][kept]].\n"))
         (requests (gdocs-diff-generate old new)))
    (should requests)
    (should (cl-every (lambda (request) (alist-get 'updateTextStyle request)) requests))
    (let ((update (alist-get 'updateTextStyle (car requests))))
      (should (equal (alist-get 'fields update) "link"))
      (should (equal (alist-get 'range update) '((startIndex . 4) (endIndex . 5))))
      (should-not (assq 'link (alist-get 'textStyle update))))
    (let ((result (gdocs-link-mutation-test--native (append initial requests))))
      (should (equal (gdocs-convert-ir-to-org result) "old [[https://example.org][kept]].\n"))
      (should-not (gdocs-diff-generate result new)))))

(ert-deftest gdocs-link-mutation-exact-clears-precede-word-edits-and-authored-restoration ()
  "Touched paragraph links are cleared at old coordinates before text moves."
  (let* ((initial (gdocs-link-mutation-test--dirty-requests))
         (old (gdocs-link-mutation-test--native initial))
         (new (gdocs-convert-org-string-to-ir "changed 😀 [[https://example.org][kept]].\n"))
         (requests (gdocs-diff-generate old new))
         (before (cl-subseq requests 0 2)))
    (should (equal (mapcar (lambda (request) (alist-get 'range (alist-get 'updateTextStyle request))) before)
                   '(((startIndex . 4) (endIndex . 5)) ((startIndex . 5) (endIndex . 9)))))
    (dolist (request before) (should (equal (alist-get 'fields (alist-get 'updateTextStyle request)) "link")))
    (let ((result (gdocs-link-mutation-test--native (append initial requests))))
      (should (equal (gdocs-convert-ir-to-org result) "changed 😀 [[https://example.org][kept]].\n"))
      (should-not (gdocs-diff-generate result new)))))

(ert-deftest gdocs-link-mutation-intentional-whitespace-link-is-not-stripped ()
  "Explicit source intent protects whitespace links regardless of plist order."
  (let* ((initial (gdocs-link-mutation-test--dirty-requests))
         (old (gdocs-link-mutation-test--native initial))
         (new (copy-tree old t)))
    (plist-put (car new) :contents '((:text "old") (:link "https://inherited.invalid" :text " ")
                                     (:text "kept" :link "https://example.org") (:text ".")))
    (should-not (gdocs-diff-generate old new))
    (should-not (gdocs-diff-generate old (gdocs-convert-org-string-to-ir
                                          "old[[https://inherited.invalid][ ]][[https://example.org][kept]].\n")))))

(ert-deftest gdocs-link-mutation-fragmented-authored-link-keeps-internal-spaces ()
  "Style fragmentation inside a link label cannot make its spaces look stray."
  (let* ((source "[[https://example.org][one two]].\n")
         (initial (append (gdocs-convert-ir-to-docs-requests (gdocs-convert-org-string-to-ir source))
                          '(((updateTextStyle . ((textStyle . ((foregroundColor . ((color . ((rgbColor . ((red . 0.31) (green . 0.27) (blue . 0.91)))))))))
                                                 (range . ((startIndex . 4) (endIndex . 5))) (fields . "foregroundColor")))))))
         (old (gdocs-link-mutation-test--native initial))
         (spans (plist-get (car old) :native-link-spans)))
    (should (seq-some (lambda (element) (seq-some (lambda (part)
                                                    (and (equal (alist-get 'content (alist-get 'textRun part)) " ")
                                                         (alist-get 'link (alist-get 'textStyle (alist-get 'textRun part)))))
                                                  (alist-get 'elements (alist-get 'paragraph element))))
                      (alist-get 'content (alist-get 'body (gdocs-fidelity-test--returned-json initial)))))
    (should (= (length spans) 1))
    (should (equal (plist-get (car spans) :text) "one two"))
    (should-not (gdocs-diff-generate old (gdocs-convert-org-string-to-ir source)))))

(ert-deftest gdocs-link-mutation-container-cell-links-are-not-cleared-by-root-modify ()
  "A partial container update cannot clear authored links from untouched cells."
  (let ((old '((:type table :native-link-spans ((:start 3 :end 7 :url "https://example.org" :text "kept")))))
        (new '((:type table))))
    (should-not (gdocs-diff-links--plan old new '((:op modify :old-index 0 :new-index 0)) 1))))

(ert-deftest gdocs-link-mutation-body-metadata-does-not-cross-segment-namespaces ()
  "Equal numeric offsets in body and native note segments are not interchangeable."
  (let* ((note (list :type 'footnote :segment-id "note-segment" :doc-start 1 :doc-end 50))
         (body (alist-get 'content (alist-get 'body (gdocs-fidelity-test--returned-json
                                                     (gdocs-link-mutation-test--dirty-requests))))))
    (gdocs-convert-links-annotate-native-ir (list note) body)
    (should-not (plist-get note :native-link-spans))))

(ert-deftest gdocs-link-mutation-live-seeded-span-is-addressed-before-utf16-word-edit ()
  "Actual native data plans exact old spans before shifting an authored hyperlink."
  (let* ((old (gdocs-link-mutation-test--before-paragraph (gdocs-link-mutation-test--live "before")))
         (new (gdocs-convert-org-string-to-ir "Before *finished 🧭* prose with [[https://example.org/preserved][kept link]] 😀.\n"))
         (requests (gdocs-diff-generate (list old) new (plist-get old :doc-start))))
    (should (equal (mapcar (lambda (request) (alist-get 'range (alist-get 'updateTextStyle request)))
                           (cl-subseq requests 0 2))
                   '(((startIndex . 64) (endIndex . 73)) ((startIndex . 73) (endIndex . 74)))))
    (should (seq-some (lambda (request)
                        (let ((style (alist-get 'updateTextStyle request)))
                          (and (equal (alist-get 'range style) '((startIndex . 68) (endIndex . 77)))
                               (equal (alist-get 'url (alist-get 'link (alist-get 'textStyle style)))
                                      "https://example.org/preserved")))) requests))))

(ert-deftest gdocs-link-mutation-live-result-preserves-authored-runs-and-converges ()
  "Actual post-edit native runs contain only intended URLs and correct Unicode emphasis."
  (let* ((native (gdocs-link-mutation-test--live "after"))
         (paragraph (gdocs-link-mutation-test--before-paragraph native))
         (source "Before *finished 🧭* prose with [[https://example.org/preserved][kept link]] 😀.\n")
         (spans (plist-get paragraph :native-link-spans)))
    (should (= (length spans) 1))
    (should (equal (plist-get (car spans) :text) "kept link"))
    (should (equal (plist-get (car spans) :url) "https://example.org/preserved"))
    (should (equal (gdocs-convert-ir-to-org (list paragraph)) source))
    (should-not (gdocs-diff-generate (list paragraph) (gdocs-convert-org-string-to-ir source)
                                     (plist-get paragraph :doc-start)))
    (should (= (seq-count #'gdocs-convert-section-break-element-p native) 4))))

(provide 'gdocs-link-mutation-test)
;;; gdocs-link-mutation-test.el ends here
