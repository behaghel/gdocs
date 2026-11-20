;;; gdocs-convert-status-test.el --- Static status conversion contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; Static status semantics stay independent of configurable presentation.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gdocs-convert)
(require 'gdocs-diff)

(defvar gdocs-status-default-styles)

(defun gdocs-convert-test--status-response (requests fragments &optional heading)
  "Build representative fetched JSON from REQUESTS and text FRAGMENTS.
Google may split or merge text runs independently of semantic ranges.  HEADING
supplies a heading level whose uniform presentation is bold."
  (let ((index 1) (counter 0) elements named-ranges)
    (dolist (text fragments)
      (let ((end (+ index (gdocs-convert--string-to-utf16-length text))))
        (push `((startIndex . ,index) (endIndex . ,end)
                (textRun . ((content . ,text) (textStyle . ((bold . ,(if heading t :json-false))))))) elements)
        (setq index end)))
    (dolist (request requests)
      (when-let* ((marker (alist-get 'createNamedRange request)))
        (push (cons (intern (alist-get 'name marker))
                    `((namedRanges . [((namedRangeId . ,(format "fixture-%d" (cl-incf counter)))
                                       (ranges . [,(alist-get 'range marker)]))]))) named-ranges)))
    `((body . ((content . [((startIndex . 1) (endIndex . ,index)
                            (paragraph . ((paragraphStyle . ((namedStyleType . ,(if heading (format "HEADING_%d" heading) "NORMAL_TEXT"))))
                                          (elements . ,(vconcat (nreverse elements))))))])))
      (namedRanges . ,named-ranges))))

(ert-deftest gdocs-status-conversion-has-a-dedicated-module ()
  "Status parsing, rendering, and provider import share one module."
  (dolist (function '(gdocs-convert--status-link-to-run
                      gdocs-convert--status-run-to-org
                      gdocs-convert--docs-status-text-parts))
    (should (equal (file-name-base (symbol-file function 'defun))
                   "gdocs-convert-status"))))

(ert-deftest gdocs-status-palette-overrides-change-requests-not-identity ()
  "A configured palette changes appearance without changing IR or markers."
  (let* ((source "[[status:Green][Ready 😀]]\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (before (copy-tree ir))
         (gdocs-status-default-styles
          '((green :bold nil :foreground-color "#654321" :background-color "#123456")))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (style (seq-some
                 (lambda (request)
                   (let ((style (alist-get 'textStyle (alist-get 'updateTextStyle request))))
                     (when (alist-get 'backgroundColor style) style))) requests))
         (marker (seq-some (lambda (request) (alist-get 'createNamedRange request)) requests)))
    (should (equal (alist-get 'backgroundColor style)
                   (gdocs-convert--hex-to-color "#123456")))
    (should (equal (alist-get 'foregroundColor style)
                   (gdocs-convert--hex-to-color "#654321")))
    (should (equal ir before))
    (should (equal (gdocs-convert-ir-to-org ir) source))
    (should (equal (alist-get 'name marker) "gdocs-org-marker:status:run-1:status:Green:1"))
    (should-not (gdocs-diff-generate ir ir))))

(ert-deftest gdocs-convert-test-status-links-are-semantic-not-urls ()
  "Canonical and legacy status links retain colour and label instead of a URL."
  (dolist (protocol '("status" "confluence-status"))
    (let* ((source (format "Before [[%s:Green][Ready 😀]] after.\n" protocol))
           (ir (gdocs-convert-org-string-to-ir source))
           (runs (plist-get (car ir) :contents))
           (status (seq-find (lambda (run) (plist-get run :status)) runs)))
      (should status)
      (should (equal (plist-get status :text) "Ready 😀"))
      (should (equal (plist-get status :status)
                     (list :link-type protocol :color "Green" :described t)))
      (should-not (plist-get status :link))
      (should (equal (gdocs-convert-ir-to-org ir) source)))))

(ert-deftest gdocs-convert-test-status-requests-style-and-mark-exact-utf16-range ()
  "Status labels receive colour and semantic metadata, with inherited links reset."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "😀 Before [[confluence-status:Green][Ready 😀]] after.\n"))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (update (seq-some
                  (lambda (request)
                    (let ((data (alist-get 'updateTextStyle request)))
                      (when (alist-get 'backgroundColor (alist-get 'textStyle data)) data)))
                  requests))
         (marker (seq-some (lambda (request) (alist-get 'createNamedRange request)) requests)))
    (should update)
    (should (alist-get 'foregroundColor (alist-get 'textStyle update)))
    (should (string-match-p "link" (alist-get 'fields update)))
    (should-not (alist-get 'link (alist-get 'textStyle update)))
    (should (equal (alist-get 'range update) '((startIndex . 11) (endIndex . 19))))
    (should marker)
    (should (string-prefix-p "gdocs-org-marker:status:" (alist-get 'name marker)))
    (should (string-suffix-p ":confluence-status:Green:1" (alist-get 'name marker)))
    (should (equal (alist-get 'range marker) '((startIndex . 11) (endIndex . 19))))))

(ert-deftest gdocs-convert-test-status-imports-from-inside-a-merged-text-run ()
  "Named-range identity restores a status even when Google merges text runs."
  (let* ((json (json-parse-string
                (concat "{\"body\":{\"content\":[{\"startIndex\":1,\"endIndex\":27,"
                        "\"paragraph\":{\"elements\":[{\"startIndex\":1,\"endIndex\":27,"
                        "\"textRun\":{\"content\":\"😀 Before Ready 😀 after.\\n\"}}]}}]},"
                        "\"namedRanges\":{\"gdocs-org-marker:status:run-11:confluence-status:Green:1\":{"
                        "\"namedRanges\":[{\"namedRangeId\":\"status-1\",\"ranges\":[{\"startIndex\":11,\"endIndex\":19}]}]}}}")
                :object-type 'alist))
         (ir (gdocs-convert-docs-json-to-ir json)))
    (should (equal (gdocs-convert-ir-to-org ir)
                   "😀 Before [[confluence-status:Green][Ready 😀]] after.\n"))
    (should (equal (plist-get (seq-find (lambda (run) (plist-get run :status))
                                        (plist-get (car ir) :contents)) :status-marker-ids)
                   '("status-1")))
    (should-not (plist-get (car ir) :gdocs-marker))))

(ert-deftest gdocs-convert-test-status-lifecycle-and-repeat-diff ()
  "Create/request/fetch/pull retains static status identity across run boundaries."
  (require 'gdocs-diff)
  (dolist (case '(("Before [[confluence-status:Green][Ready]] after.\n"
                   ("Before Re" "ady" " after.\n"))
                  ("[[status:Purple][One]][[status:Purple][Two]]\n" ("OneTwo\n"))
                  ("[[status:Gray]]\n" ("Gray\n"))))
    (let* ((source (car case))
           (local (gdocs-convert-org-string-to-ir source))
           (requests (gdocs-convert-ir-to-docs-requests local))
           (json (gdocs-convert-test--status-response requests (cadr case)))
           (remote (gdocs-convert-docs-json-to-ir json))
           (rendered (gdocs-convert-ir-to-org remote)))
      (should (equal rendered source))
      (should-not (gdocs-diff-generate remote local))
      (should-not (gdocs-diff-generate remote (gdocs-convert-org-string-to-ir rendered))))))

(ert-deftest gdocs-convert-test-status-remote-label-edit-retains-colour-and-protocol ()
  "Editing a status label in Google does not lose its original colour or scheme."
  (let* ((local (gdocs-convert-org-string-to-ir "Before [[confluence-status:Green][Ready]] after.\n"))
         (requests (gdocs-convert-ir-to-docs-requests local))
         (json (gdocs-convert-test--status-response requests '("Before Doing after.\n"))))
    (should (equal (gdocs-convert-ir-to-org (gdocs-convert-docs-json-to-ir json))
                   "Before [[confluence-status:Green][Doing]] after.\n"))))

(ert-deftest gdocs-convert-test-status-heading-does-not-reintroduce-bold-markup ()
  "Generated status styling must not make surrounding heading bold semantic."
  (let* ((source "*** Head [[status:Green][Ready]]\n")
         (requests (gdocs-convert-ir-to-docs-requests (gdocs-convert-org-string-to-ir source)))
         (json (gdocs-convert-test--status-response requests '("Head Ready\n") 3)))
    (should (equal (gdocs-convert-ir-to-org (gdocs-convert-docs-json-to-ir json)) source))))

(ert-deftest gdocs-convert-test-status-unsupported-source-fails-before-requests ()
  "Unsupported status colours, rich labels, and native mixtures cannot flatten."
  (dolist (source '("[[status:Orange][Ready]]\n"
                    "[[status:Green][*Ready*]]\n"
                    "| [[status:Green][Ready]] |\n"
                    "[fn:x] [[status:Green][Ready]]\n"
                    "Today <2026-11-12 Thu> [[status:Green][Ready]].\n"))
    (should-error (gdocs-convert-org-string-to-ir source) :type 'user-error)))

(ert-deftest gdocs-convert-test-status-remote-edit-of-undescribed-label-is-stable ()
  "A renamed bare status becomes described without a perpetual metadata diff."
  (require 'gdocs-diff)
  (let* ((requests (gdocs-convert-ir-to-docs-requests
                    (gdocs-convert-org-string-to-ir "[[status:Gray]]\n")))
         (json (gdocs-convert-test--status-response requests '("Done\n")))
         (remote (gdocs-convert-docs-json-to-ir json))
         (rendered (gdocs-convert-ir-to-org remote)))
    (should (equal rendered "[[status:Gray][Done]]\n"))
    (should-not (gdocs-diff-generate remote (gdocs-convert-org-string-to-ir rendered)))))

(ert-deftest gdocs-convert-test-status-import-refuses-table-or-invalid-label ()
  "Moving a marked pill into unsupported structure cannot silently discard it."
  (let* ((requests (gdocs-convert-ir-to-docs-requests
                    (gdocs-convert-org-string-to-ir "[[status:Green][Ready]]\n")))
         (json (gdocs-convert-test--status-response requests '("Ready\n")))
         (paragraph (aref (alist-get 'content (alist-get 'body json)) 0)))
    (setf (alist-get 'content (alist-get 'body json))
          (vector `((startIndex . 1) (endIndex . 30)
                    (table . ((tableRows . [((tableCells . [((content . [,paragraph]))]))]))))))
    (should-error (gdocs-convert-docs-json-to-ir json) :type 'user-error))
  (dolist (label '("  " "x]]"))
    (let ((run (plist-put (gdocs-convert--make-plain-run label) :status
                          '(:link-type "status" :color "Green" :described t))))
      (should-error (gdocs-convert--run-to-org run) :type 'user-error))))

(ert-deftest gdocs-convert-test-status-colours-and-link-registration ()
  "All supported colours work independently of custom Org link registration."
  (let ((saved (copy-tree org-link-parameters)))
    (unwind-protect
        (progn
          (org-link-set-parameters "confluence-status" :follow #'ignore)
          (dolist (color '("Grey" "Gray" "Red" "Yellow" "Green" "Blue" "Purple"))
            (let* ((source (format "[[confluence-status:%s][Ready]]\n" color))
                   (ir (gdocs-convert-org-string-to-ir source))
                   (requests (gdocs-convert-ir-to-docs-requests ir)))
              (should (equal (gdocs-convert-ir-to-org ir) source))
              (should (seq-some (lambda (request)
                                  (alist-get 'backgroundColor
                                             (alist-get 'textStyle (alist-get 'updateTextStyle request))))
                                requests)))))
      (setq org-link-parameters saved)
      (org-link-make-regexps))))

(ert-deftest gdocs-status-uniform-small-caps-preserves-source-case-and-native-edits ()
  "Presentation removes tall capitals without losing source label spelling."
  (let* ((case-fold-search nil)
         (gdocs-style-definitions '((gdocs-status-green :text (:small-caps t))))
         (source "State: [[status:green][Ready now Bépo 🧭]].\n")
         (local (gdocs-convert-org-string-to-ir source))
         (requests (gdocs-convert-ir-to-docs-requests local))
         (text (mapconcat (lambda (request) (or (alist-get 'text (alist-get 'insertText request)) "")) requests ""))
         (native (gdocs-convert-docs-json-to-ir
                  (gdocs-convert-test--status-response requests '("State: rea" "dy now bépo 🧭.\n")))))
    (should (equal text "State: ready now bépo 🧭.\n"))
    (should (equal (gdocs-convert-ir-to-org native) source))
    (should-not (gdocs-diff-generate native local))
    (require 'gdocs-fidelity-corrections-test)
    (let* ((next-source (string-replace "Ready now" "Next step" source))
           (next (gdocs-convert-org-string-to-ir next-source))
           (base (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json requests)))
           (changes (gdocs-diff-generate base next))
           (inserted (mapconcat (lambda (request) (or (alist-get 'text (alist-get 'insertText request)) "")) changes ""))
           (returned (gdocs-convert-docs-json-to-ir (gdocs-fidelity-test--returned-json (append requests changes)))))
      (should (string-match-p "next step bépo" inserted))
      (should-not (string-match-p "Next step" inserted))
      (should (equal (gdocs-convert-ir-to-org returned) next-source))
      (should-not (gdocs-diff-generate returned next)))
    (let ((edited (gdocs-convert-docs-json-to-ir
                   (gdocs-convert-test--status-response requests '("State: next step bépo 🧭.\n")))))
      (should (equal (gdocs-convert-ir-to-org edited) "State: [[status:green][next step bépo 🧭]].\n")))))

(ert-deftest gdocs-status-uniform-case-provenance-is-validated-before-publication ()
  "Oversized or malformed source spelling cannot make a lossy status chip."
  (let ((gdocs-style-definitions '((gdocs-status-green :text (:small-caps t)))))
    (should-error (gdocs-convert-org-string-to-ir
                   (format "[[status:green][%s]]" (make-string 80 ?🧭))) :type 'user-error))
  (dolist (data '("status:green:1:v2:%ZZ" "status:green:1:v2:" "status:green:1:v3:Ready"))
    (should-error (gdocs-convert--status-marker-data (list :data data)) :type 'user-error)))

(provide 'gdocs-convert-status-test)
;;; gdocs-convert-status-test.el ends here
