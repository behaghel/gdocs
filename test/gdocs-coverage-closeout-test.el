;;; gdocs-coverage-closeout-test.el --- Mixed native coverage captures -*- lexical-binding: t; -*-

;;; Commentary:
;; Real sanitized create/edit responses complement isolated feature fixtures.
;; The fixture's local image is enriched with a known public URI without I/O;
;; the conventional footnotes heading is adapter chrome, not document body.

;;; Code:

(require 'ert)
(require 'gdocs-sync)
(require 'gdocs-disposable-plan)

(defconst gdocs-coverage-test--directory (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing mixed conformance fixtures.")

(defun gdocs-coverage-test--source (stage)
  "Read the synthetic source fixture at STAGE."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name
      (pcase stage
        ("created" "fixtures/disposable-coverage-conformance.org")
        ("edited" "fixtures/disposable-coverage-conformance-edited.org")
        ("presentation-created" "fixtures/disposable-coverage-presentation.org")
        ("presentation-edited" "fixtures/disposable-coverage-presentation-edited.org")
        ("polished" "fixtures/disposable-coverage-polished.org")
        ("mini-created" "fixtures/disposable-status-term-create.org")
        (_ (error "Unknown coverage fixture stage: %s" stage)))
      gdocs-coverage-test--directory))
    (buffer-string)))

(defun gdocs-coverage-test--local (stage)
  "Convert source STAGE with explicit image enrichment and native-note body filtering."
  (let ((ir (seq-remove (lambda (element)
                          (and (eq (plist-get element :style) 'heading-1)
                               (equal (gdocs-convert--runs-to-plain-text (plist-get element :contents)) "Footnotes")))
                        (gdocs-convert-org-string-to-ir (gdocs-coverage-test--source stage)))))
    (dolist (element ir)
      (when (eq (plist-get element :type) 'image)
        (plist-put element :uri "https://www.gstatic.com/images/branding/googlelogo/1x/googlelogo_color_272x92dp.png")))
    ir))

(defun gdocs-coverage-test--native (stage)
  "Read and decode the actual sanitized response at STAGE."
  (let* ((json-object-type 'alist) (json-array-type 'vector)
         (document (json-read-file (expand-file-name (format "fixtures/live-coverage-%s.json" stage)
                                                     gdocs-coverage-test--directory))))
    (should-not (alist-get 'revisionId document))
    (gdocs-sync--filter-empty-paragraphs (gdocs-disposable-plan--body (gdocs-convert-docs-json-to-ir document)))))

(ert-deftest gdocs-coverage-real-mixed-create-and-edit-have-no-phantom-changes ()
  "Every supported feature coexists without false mutation anchors or repeat writes."
  (let ((gdocs-convert-footnote-reference-text-function (lambda (_object) "⟦coverage-native-note⟧")))
    (dolist (stage '("created" "edited" "presentation-created" "presentation-edited" "polished"))
      (let* ((native (gdocs-coverage-test--native stage)) (local (gdocs-coverage-test--local stage))
             (operations (gdocs-diff-compute-operations native local)))
        (dolist (kind '(table image source-block example-block quote-block callout-block center-block verse-block horizontal-rule footnote))
          (should (seq-some (lambda (element) (eq (plist-get element :type) kind)) native)))
        (should (cl-every (lambda (op) (eq (plist-get op :op) 'keep)) operations))
        (should-not (gdocs-diff-generate native local))
        (should (string-match-p "10:30" (gdocs-convert-ir-to-org native)))))))

(ert-deftest gdocs-coverage-mixed-edit-keeps-date-chips-and-native-note-identity ()
  "Multi-feature edits do not recreate unchanged chips or existing note references."
  (let* ((gdocs-convert-footnote-reference-text-function (lambda (_object) "⟦coverage-native-note⟧"))
         (old (gdocs-coverage-test--native "created")) (new (gdocs-coverage-test--local "edited"))
         (requests (gdocs-diff-generate old new)))
    (should requests)
    (should-not (seq-some (lambda (request) (or (alist-get 'insertDate request) (alist-get 'createFootnote request))) requests))
    (should (seq-some (lambda (request)
                        (or (alist-get 'segmentId (alist-get 'range (alist-get 'deleteContentRange request)))
                            (alist-get 'segmentId (alist-get 'location (alist-get 'insertText request))))) requests))))

(ert-deftest gdocs-coverage-export-redacts-transient-image-locators ()
  "Tracked evidence retains authored image identity, not temporary native URLs."
  (let* ((directory (make-temp-file "gdocs-coverage-export-" t))
         (source (expand-file-name "remote.json" directory))
         (destination (expand-file-name "fixture.json" directory)))
    (unwind-protect
        (progn
          (with-temp-file source
            (insert "{\"documentId\":\"synthetic-document\",\"revisionId\":\"synthetic-revision\",\"inlineObjects\":{\"image\":{\"imageProperties\":{\"contentUri\":\"https://example.org/transient-native-image\",\"sourceUri\":\"https://example.org/authored-image.png\"}}}}"))
          (gdocs-disposable-plan-export-fixture source destination)
          (with-temp-buffer
            (insert-file-contents destination)
            (should-not (string-match-p "transient-native-image" (buffer-string)))
            (should (string-match-p "authored-image.png" (buffer-string)))))
      (delete-directory directory t))))

(ert-deftest gdocs-coverage-real-panels-and-small-caps-retain-source-semantics ()
  "Actual native styles separate literal panels without capitalizing stored labels."
  (dolist (stage '("presentation-created" "presentation-edited"))
    (let* ((json-object-type 'alist) (json-array-type 'vector)
           (document (json-read-file (expand-file-name (format "fixtures/live-coverage-%s.json" stage)
                                                       gdocs-coverage-test--directory)))
           (paragraphs (delq nil (mapcar (lambda (item) (alist-get 'paragraph item))
                                         (append (alist-get 'content (alist-get 'body document)) nil))))
           (text (lambda (paragraph)
                   (mapconcat (lambda (element) (or (alist-get 'content (alist-get 'textRun element)) ""))
                              (alist-get 'elements paragraph) "")))
           (last-source (cl-position-if (lambda (paragraph)
                                          (string-prefix-p "Code blank line retained" (funcall text paragraph))) paragraphs))
           (gap (nth (1+ last-source) paragraphs))
           (style (alist-get 'paragraphStyle gap))
           (label (if (equal stage "presentation-created") "Ready" "Ready now"))
           (status (seq-find (lambda (run) (equal (alist-get 'content run) label))
                             (apply #'append (mapcar (lambda (paragraph)
                                                       (mapcar (lambda (element) (alist-get 'textRun element))
                                                               (append (alist-get 'elements paragraph) nil))) paragraphs))))
           (org (gdocs-convert-ir-to-org (gdocs-coverage-test--native stage))))
      (should (equal (funcall text gap) "\n"))
      (should-not (alist-get 'color (alist-get 'backgroundColor (alist-get 'shading style))))
      (dolist (side '(borderTop borderBottom borderLeft borderRight))
        (let ((width (alist-get 'width (alist-get side style))))
          (should (equal (alist-get 'unit width) "PT"))
          ;; Native proto JSON omits a zero magnitude, not the width object.
          (should (zerop (or (alist-get 'magnitude width) 0)))))
      (should (eq (alist-get 'smallCaps (alist-get 'textStyle status)) t))
      (should (string-match-p (regexp-quote (format "[[status:green][%s]]" label)) org))
      (should (string-match-p "- Term ::" org))
      (should-not (string-match-p (regexp-quote "*Term*") org)))))

(ert-deftest gdocs-coverage-uniform-caps-term-provenance-and-brown-example-live-captures ()
  "Actual create/push retain plain terms and source spelling behind styled glyphs."
  (dolist (stage '("polished" "mini-created"))
    (let* ((native (gdocs-coverage-test--native stage))
           (local (gdocs-coverage-test--local stage))
           (org (gdocs-convert-ir-to-org native))
           (json-object-type 'alist) (json-array-type 'vector)
           (document (json-read-file (expand-file-name (format "fixtures/live-coverage-%s.json" stage)
                                                       gdocs-coverage-test--directory)))
           (paragraphs (delq nil (mapcar (lambda (element) (alist-get 'paragraph element))
                                         (append (alist-get 'content (alist-get 'body document)) nil))))
           (text (lambda (paragraph)
                   (mapconcat (lambda (element) (or (alist-get 'content (alist-get 'textRun element)) ""))
                              (alist-get 'elements paragraph) "")))
           (term (seq-find (lambda (paragraph) (string-prefix-p "Term ::" (funcall text paragraph))) paragraphs))
           (example (seq-find (lambda (paragraph) (or (string-prefix-p "Example literal" (funcall text paragraph))
                                                      (string-prefix-p "Brown example" (funcall text paragraph)))) paragraphs))
           (rgb (alist-get 'rgbColor (alist-get 'color (alist-get 'backgroundColor (alist-get 'shading (alist-get 'paragraphStyle example)))))))
      (should (eq (alist-get 'bold (alist-get 'textStyle (alist-get 'textRun (aref (alist-get 'elements term) 0)))) t))
      (should (equal (mapcar (lambda (key) (round (* 255 (or (alist-get key rgb) 0)))) '(red green blue)) '(200 181 154)))
      (dolist (paragraph paragraphs)
        (seq-doseq (element (alist-get 'elements paragraph))
          (when-let* ((run (alist-get 'textRun element)) (small (alist-get 'smallCaps (alist-get 'textStyle run))))
            (when (eq small t)
              (should (equal (alist-get 'content run) (downcase (alist-get 'content run))))))))
      (should (string-match-p "- Term ::" org))
      (should-not (string-match-p (regexp-quote "*Term*") org))
      (when (equal stage "mini-created") (should (string-match-p (regexp-quote "*Emphasis*") org)))
      (should (equal (gdocs-diff--element-keys native) (gdocs-diff--element-keys local)))
      (should-not (gdocs-diff-generate native local)))))

(provide 'gdocs-coverage-closeout-test)
;;; gdocs-coverage-closeout-test.el ends here
