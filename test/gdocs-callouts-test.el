;;; gdocs-callouts-test.el --- Callout ownership and fidelity -*- lexical-binding: t; -*-

;;; Commentary:
;; Full local publication/returned-JSON flows exercise semantic callouts.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gdocs-fidelity-corrections-test)

(ert-deftest gdocs-callouts-list-chrome-retains-columns-and-background-edge ()
  "Hanging list columns must not create notches in the full-width callout surface."
  (let* ((surface '(:space-above 0 :space-below 0 :indent-start 0 :indent-first-line 0
                                 :background-color "#FBEDE8" :border-color "#FBEDE8" :border-padding 6))
         (gdocs-style-definitions
          (append (mapcar (lambda (role) (list (intern (format "gdocs-callout-note-%s" role)) :paragraph surface))
                          '(label first line last single first-compact line-compact last-compact single-compact))
                  gdocs-style-definitions))
         (source "#+ATTR_CALLOUT: :type note\n#+BEGIN_CALLOUT\n- Native\n\n- [ ] Task\n\n  Continue\n#+END_CALLOUT\n")
         (json (gdocs-fidelity-test--returned-json
                (gdocs-convert-ir-to-docs-requests (gdocs-convert-org-string-to-ir source))))
         (seen 0))
    (dolist (element (alist-get 'content (alist-get 'body json)))
      (let* ((paragraph (alist-get 'paragraph element))
             (text (mapconcat (lambda (item) (or (alist-get 'content (alist-get 'textRun item)) ""))
                              (alist-get 'elements paragraph) ""))
             (style (alist-get 'paragraphStyle paragraph)))
        (when (member text '("Native\n" "☐ Task\n" "Continue\n"))
          (cl-incf seen)
          (let ((first (alist-get 'magnitude (alist-get 'indentFirstLine style)))
                (body (alist-get 'magnitude (alist-get 'indentStart style)))
                (padding (alist-get 'magnitude (alist-get 'padding (alist-get 'borderLeft style)))))
            (should (= body 36))
            (should (= first (if (equal text "Continue\n") 36 18)))
            ;; The border/shading rectangle includes its padding: keep its
            ;; left edge at the same -6 pt as ordinary callout paragraphs.
            (should (= (- (min first body) padding) -6))))))
    (should (= seen 3))))

(ert-deftest gdocs-callouts-physical-owners ()
  "Callout recognition, conversion, geometry and list repair have a feature owner."
  (dolist (function '(gdocs-convert--callout-block-p gdocs-convert--callout-block-to-ir
                                                     gdocs-convert--callout-block-to-requests gdocs-convert--attach-callout-block-markers
                                                     gdocs-convert--ir-callout-block-to-org gdocs-convert--callout-list-marker-data
                                                     gdocs-convert--callout-list-post-bullet-indent-resets))
    (should (string-match-p "gdocs-convert-callouts\\.elc?\\'" (symbol-file function 'defun))))
  (dolist (function '(gdocs-diff--callout-block-key gdocs-diff--callout-block-utf16-length))
    (should (string-match-p "gdocs-diff-callouts\\.elc?\\'" (symbol-file function 'defun)))))

(ert-deftest gdocs-callouts-unicode-title-and-paragraphs-full-flow ()
  "Unicode titles and lowercase paragraphs survive publication, fetch and repeats."
  (let* ((source "#+ATTR_CALLOUT: :type warning :title \"Audit 😀\"\n#+BEGIN_CALLOUT\n*First* [[https://example.org][link]].\n\nsecond paragraph.\n#+END_CALLOUT\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (native (gdocs-fidelity-test--returned-json (gdocs-convert-ir-to-docs-requests ir)))
         (remote (gdocs-convert-docs-json-to-ir native))
         (again (gdocs-convert-org-string-to-ir (gdocs-convert-ir-to-org remote))))
    (should (equal (plist-get (car remote) :title) "Audit 😀"))
    (should (= (length (plist-get (car remote) :body-elements)) 2))
    (should (equal (gdocs-convert-ir-to-org remote) source))
    (should-not (gdocs-diff-generate remote ir))
    (should-not (gdocs-diff-generate remote again))
    (should (gdocs-diff-generate remote (gdocs-convert-org-string-to-ir (string-replace "second" "changed" source))))))

(ert-deftest gdocs-callouts-checklist-and-literal-prefix-full-flow ()
  "Checked/partial tasks and literal list-looking body prefixes retain semantics."
  (let* ((source "#+ATTR_CALLOUT: :type info\n#+BEGIN_CALLOUT\n- [X] done\n- [-] partial\n- [ ] pending\n- - literal prefix\n#+END_CALLOUT\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (remote (gdocs-convert-docs-json-to-ir
                  (gdocs-fidelity-test--returned-json (gdocs-convert-ir-to-docs-requests ir)))))
    (should (equal (gdocs-convert-ir-to-org remote) source))
    (should-not (gdocs-diff-generate remote ir))))

(ert-deftest gdocs-callouts-new-label-is-a-block-boundary ()
  "A second label starts a new callout even when data matches and spacing is removed."
  (cl-labels ((paragraph (kind id text)
                (list :type 'paragraph :style 'normal :contents (list (list :text text))
                      :gdocs-marker (list (list :type kind :id id :data "info:")))))
    (let ((ir (list (paragraph 'callout-label "one" "ℹ️ Info")
                    (paragraph 'callout-body "one" "First")
                    (paragraph 'callout-label "two" "ℹ️ Info")
                    (paragraph 'callout-body "two" "second"))))
      (should (= (length (gdocs-convert--attach-callout-block-markers ir)) 2)))))

(defun gdocs-callouts-test--branch-p (tree)
  "Return non-nil if TREE recognizes callout kinds."
  (and (consp tree)
       (or (equal tree ''callout-block) (gdocs-callouts-test--branch-p (car tree))
           (gdocs-callouts-test--branch-p (cdr tree)))))

(ert-deftest gdocs-callouts-generic-diff-neutrality ()
  "Generic LCS/index algorithms consume descriptors rather than callout kinds."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "gdocs-diff.el" (file-name-directory (locate-library "gdocs-convert"))))
    (goto-char (point-min))
    (condition-case nil
        (while t (should-not (gdocs-callouts-test--branch-p (read (current-buffer)))))
      (end-of-file nil))))

(ert-deftest gdocs-callouts-native-title-edits-use-applied-label-provenance ()
  "Remote title edits survive even if label configuration changes before pull."
  (let* ((source "#+ATTR_CALLOUT: :type info :title \"Old 😀\"\n#+BEGIN_CALLOUT\nBody.\n#+END_CALLOUT\n")
         (ir (gdocs-convert-org-string-to-ir source))
         (gdocs-callout-labels '(("info" . "Custom — heading")))
         (json (gdocs-fidelity-test--returned-json (gdocs-convert-ir-to-docs-requests ir)))
         (label (alist-get 'paragraph (car (alist-get 'content (alist-get 'body json))))))
    (dolist (element (alist-get 'elements label))
      (when-let* ((run (alist-get 'textRun element)))
        (setf (alist-get 'content run) (string-replace "Old" "New" (alist-get 'content run)))))
    (let* ((gdocs-callout-labels '(("info" . "Different label")))
           (remote (gdocs-convert-docs-json-to-ir json))
           (expected (string-replace "Old" "New" source)))
      (should (equal (gdocs-convert-ir-to-org remote) expected))
      (should-not (gdocs-diff-generate remote (gdocs-convert-org-string-to-ir expected))))))

(ert-deftest gdocs-callouts-invalid-list-metadata-fails-closed ()
  "Malformed legacy and versioned list metadata cannot silently normalize away."
  (dolist (data (list "bullet:garbage" "v3:bullet:0"
                      (concat "v2:" (url-hexify-string "[1]"))
                      (concat "v2:" (url-hexify-string "{\"type\":\"bullet\",\"level\":0,\"checked\":\"yes\"}"))))
    (should-error (gdocs-convert--callout-list-marker-data (list :data data)) :type 'user-error)))

(ert-deftest gdocs-callouts-style-repair-is-revision-guarded-and-content-free ()
  "A disposable style repair cannot replace text or apply to different semantics."
  (require 'gdocs-disposable-plan)
  (let* ((directory (make-temp-file "gdocs-style-repair-" t))
         (source-file (expand-file-name "source.org" directory))
         (remote-file (expand-file-name "remote.json" directory))
         (source "#+BEGIN_CALLOUT\nParagraph.\n\n- [ ] Task\n#+END_CALLOUT\n")
         (gdocs-style-definitions
          (append (mapcar (lambda (role)
                            (list (intern (format "gdocs-callout-info-%s" role))
                                  :paragraph '(:background-color "#FBEDE8" :border-color "#FBEDE8" :border-padding 6)))
                          '(label first line last single first-compact line-compact last-compact single-compact))
                  gdocs-style-definitions))
         (json (gdocs-fidelity-test--returned-json
                (gdocs-convert-ir-to-docs-requests (gdocs-convert-org-string-to-ir source)))))
    (unwind-protect
        (progn
          (with-temp-file source-file (insert source))
          (setf (alist-get 'revisionId json) "fixture-revision")
          (gdocs-disposable-plan--write-json remote-file json)
          (gdocs-disposable-plan-style-repair source-file remote-file directory)
          (let* ((json-object-type 'alist) (json-key-type 'symbol) (json-array-type 'vector)
                 (body (json-read-file (expand-file-name "style-repair.requests.json" directory))))
            (should (> (length (alist-get 'requests body)) 0))
            (should (seq-every-p
                     (lambda (request)
                       (equal (alist-get 'fields (alist-get 'updateParagraphStyle request))
                              "indentStart,indentFirstLine,borderLeft"))
                     (alist-get 'requests body)))
            (should (equal (alist-get 'requiredRevisionId (alist-get 'writeControl body)) "fixture-revision")))
          (with-temp-file source-file (insert (string-replace "Paragraph" "Changed" source)))
          (should-error (gdocs-disposable-plan-style-repair source-file remote-file directory) :type 'user-error))
      (delete-directory directory t))))

(ert-deftest gdocs-callouts-inset-survives-full-list-guard-flow ()
  "Native list guards must not clear the preceding prose inset or notch shading."
  (let* ((surface '(:indent-start 4 :indent-first-line 4 :background-color "#FBEDE8"
                                  :border-color "#FBEDE8" :border-padding 4))
         (gdocs-style-definitions
          (append (mapcar (lambda (role) (list (intern (format "gdocs-callout-note-%s" role)) :paragraph surface))
                          '(label first line last single first-compact line-compact last-compact single-compact))
                  gdocs-style-definitions))
         (source "#+ATTR_CALLOUT: :type note\n#+BEGIN_CALLOUT\nFirst prose.\n\nSecond prose.\n\n- Native\n\n- [ ] Task\n#+END_CALLOUT\n")
         (requests (gdocs-convert-ir-to-docs-requests (gdocs-convert-org-string-to-ir source)))
         (json (gdocs-fidelity-test--returned-json requests))
         (last-bullet (cl-loop for request in requests for index from 0
                               when (alist-get 'createParagraphBullets request) maximize index))
         (seen 0) (prose-repairs 0))
    (dolist (element (alist-get 'content (alist-get 'body json)))
      (let* ((paragraph (alist-get 'paragraph element))
             (style (alist-get 'paragraphStyle paragraph))
             (border (alist-get 'borderLeft style)))
        (when border
          (cl-incf seen)
          (let ((first (or (alist-get 'magnitude (alist-get 'indentFirstLine style)) 0))
                (body (or (alist-get 'magnitude (alist-get 'indentStart style)) 0))
                (padding (alist-get 'magnitude (alist-get 'padding border))))
            (should (= (- (min first body) padding) 0))
            (should (>= (min first body) 4)))
          (when (string-prefix-p "First prose." (or (alist-get 'content (alist-get 'textRun (car (alist-get 'elements paragraph)))) ""))
            (cl-incf prose-repairs)
            (let* ((start (alist-get 'startIndex element))
                   (repair (cl-loop for request in requests for index from 0
                                    for update = (alist-get 'updateParagraphStyle request)
                                    when (and (= (or (alist-get 'startIndex (alist-get 'range update)) -1) start)
                                              (= (or (alist-get 'magnitude (alist-get 'indentStart (alist-get 'paragraphStyle update))) -1) 4))
                                    maximize index)))
              (should (> repair last-bullet)))))))
    (should (= prose-repairs 1))
    (should (= seen 5))))

(ert-deftest gdocs-callouts-inset-keeps-list-columns-and-surface-edge ()
  "An inset for prose cannot push list shading outside the callout surface."
  (let ((gdocs-style-definitions
         '((inset-callout :paragraph (:indent-start 4 :indent-first-line 4
                                                    :background-color "#FBEDE8" :border-color "#FBEDE8"
                                                    :border-padding 4)))))
    (dolist (membership '((:list (:type bullet :level 0))
                          (:list-continuation (:type check :level 0))))
      (let* ((paragraph (append '(:type paragraph :style normal :style-name inset-callout) membership))
             (request (gdocs-convert--callout-list-indent-reset-request 1 8 paragraph))
             (style (alist-get 'paragraphStyle (alist-get 'updateParagraphStyle request)))
             (first (alist-get 'magnitude (alist-get 'indentFirstLine style)))
             (body (alist-get 'magnitude (alist-get 'indentStart style)))
             (padding (alist-get 'magnitude (alist-get 'padding (alist-get 'borderLeft style)))))
        (should (= body 36))
        (should (= first (if (plist-get paragraph :list) 18 36)))
        (should (= (- (min first body) padding) 0))))))

(provide 'gdocs-callouts-test)
;;; gdocs-callouts-test.el ends here
