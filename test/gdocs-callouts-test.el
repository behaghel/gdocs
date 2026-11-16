;;; gdocs-callouts-test.el --- Callout ownership and fidelity -*- lexical-binding: t; -*-

;;; Commentary:
;; Full local publication/returned-JSON flows exercise semantic callouts.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gdocs-fidelity-corrections-test)

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

(provide 'gdocs-callouts-test)
;;; gdocs-callouts-test.el ends here
