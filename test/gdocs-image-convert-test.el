;;; gdocs-image-convert-test.el --- Image feature boundaries -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise image ownership through both codecs, publishing and neutral diff.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'gdocs-convert)
(require 'gdocs-diff)

(defun gdocs-image-convert-test--native ()
  "Return synthetic returned JSON with Unicode caption and source markers."
  (json-parse-string
   (concat
    "{\"body\":{\"content\":["
    "{\"startIndex\":1,\"endIndex\":2,\"paragraph\":{\"elements\":[{\"inlineObjectElement\":{\"inlineObjectId\":\"remote\"}}]}},"
    "{\"startIndex\":2,\"endIndex\":10,\"paragraph\":{\"elements\":[{\"textRun\":{\"content\":\"Logo 😀\\n\"}}]}}]},"
    "\"inlineObjects\":{\"remote\":{\"inlineObjectProperties\":{\"embeddedObject\":{\"imageProperties\":{\"contentUri\":\"https://tmp.invalid/image\",\"sourceUri\":\"https://images.invalid/logo.png\"}}}}},"
    "\"namedRanges\":{"
    "\"gdocs-org-marker:image-source:remote:img%2Flogo%20%F0%9F%98%80.png\":{\"namedRanges\":[{\"ranges\":[{\"startIndex\":1,\"endIndex\":2}]}]},"
    "\"gdocs-org-marker:image-caption:remote:org-image-caption\":{\"namedRanges\":[{\"ranges\":[{\"startIndex\":2,\"endIndex\":9}]}]}}}")
   :object-type 'alist :array-type 'list :null-object nil :false-object nil))

(ert-deftest gdocs-image-convert-full-pipeline-and-native-composition ()
  "Caption/source identity survives native decoding, publishing, edits and repeats."
  (let* ((decoder (symbol-function 'gdocs-convert-image-native-element-to-ir))
         (calls 0)
         (source "#+CAPTION: Logo 😀\n[[file:img/logo 😀.png]]\n")
         (ir (gdocs-convert-org-string-to-ir source)) remote)
    (cl-letf (((symbol-function 'gdocs-convert-image-native-element-to-ir)
               (lambda (&rest args) (setq calls (1+ calls)) (apply decoder args))))
      (setq remote (gdocs-convert-docs-json-to-ir (gdocs-image-convert-test--native))))
    (should (> calls 0))
    (should (equal (gdocs-convert-ir-to-org remote) source))
    (should (= (plist-get (car remote) :doc-end) 10))
    (should-not (gdocs-diff-generate remote ir))
    (should-not (gdocs-diff-generate remote ir))
    (should (gdocs-diff-generate remote (gdocs-convert-org-string-to-ir
                                         (string-replace "Logo" "New" source))))
    (let* ((image (plist-put (copy-sequence (car ir)) :uri "https://images.invalid/logo.png"))
           (plan (gdocs-convert--image-to-requests image 5))
           (requests (gdocs-convert-ir-to-docs-requests (list image))))
      (should (= (plist-get plan :index) 15))
      (should (alist-get 'insertInlineImage (car requests)))
      (should (seq-some (lambda (request)
                          (let ((insert (alist-get 'insertText request)))
                            (and (equal (alist-get 'text insert) "\nLogo 😀\n")
                                 (= (alist-get 'index (alist-get 'location insert)) 2)))) requests)))))

(ert-deftest gdocs-image-convert-owners-and-neutral-descriptor ()
  "Image semantics belong to image modules; diff receives neutral descriptors."
  (dolist (function '(gdocs-convert--ir-image-to-org gdocs-convert--image-to-requests
                                                     gdocs-convert--docs-inline-object-to-image-ir
                                                     gdocs-convert--attach-image-caption-markers
                                                     gdocs-convert--make-image-source-marker-request
                                                     gdocs-convert--make-image-caption-marker-requests))
    (should (string-match-p "gdocs-convert-images\\.elc?\\'" (symbol-file function 'defun))))
  (should (string-match-p "gdocs-diff-images\\.elc?\\'" (symbol-file 'gdocs-diff--image-key 'defun)))
  (let* ((image '(:type image :path "a.png" :caption "😀"))
         (descriptor (gdocs-diff-semantic-element image)))
    (should (equal (plist-get descriptor :key) (gdocs-diff--element-key image)))
    (should (= (plist-get descriptor :body-length) 1))))

(ert-deftest gdocs-image-convert-standalone-publication-plan ()
  "Image/caption planning loads without facade, transport, sync or accounts."
  (let ((directory (file-name-directory (locate-library "gdocs-convert"))))
    (with-temp-buffer
      (let* ((program '(progn
                         (require 'gdocs-convert-images)
                         (dolist (feature '(gdocs-convert gdocs-sync gdocs-api gdocs-auth))
                           (when (featurep feature) (error "Image planner loaded %s" feature)))
                         (let ((plan (gdocs-convert--image-to-requests
                                      '(:type image :uri "https://images.invalid/logo.png" :caption "😀") 5)))
                           (unless (= (plist-get plan :index) 10) (error "Image UTF-16 geometry changed")))))
             (status (call-process (expand-file-name invocation-name invocation-directory) nil t nil
                                   "--batch" "-Q" "-L" directory "--eval" (prin1-to-string program))))
        (ert-info ((buffer-string)) (should (zerop status)))))))

(defun gdocs-image-convert-test--image-branch-p (tree)
  "Return non-nil if TREE recognizes quoted image kinds."
  (and (consp tree)
       (or (equal tree ''image)
           (gdocs-image-convert-test--image-branch-p (car tree))
           (gdocs-image-convert-test--image-branch-p (cdr tree)))))

(ert-deftest gdocs-image-convert-diff-kernel-neutrality ()
  "The generic diff kernel does not classify images."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "gdocs-diff.el"
                                            (file-name-directory (locate-library "gdocs-convert"))))
    (goto-char (point-min))
    (condition-case nil
        (while t (should-not (gdocs-image-convert-test--image-branch-p (read (current-buffer)))))
      (end-of-file nil))))

(ert-deftest gdocs-image-convert-native-traversal-preserves-feature-order ()
  "A native page break/image mixture keeps first-element ordering."
  (let* ((image '((inlineObjectElement . ((inlineObjectId . "remote")))))
         (page-break '((pageBreak . ((textStyle . nil)))))
         (first (gdocs-convert--docs-special-paragraph-elements
                 (list (cons 'elements (list image page-break)))))
         (second (gdocs-convert--docs-special-paragraph-elements
                  (list (cons 'elements (list page-break image))))))
    (should (eq (plist-get first :type) 'image))
    (should (eq (plist-get second :type) 'page-break))))

(ert-deftest gdocs-image-convert-defers-unpublished-images-to-preflight ()
  "No fetchable URI means no insertion or index advance; input remains untouched."
  (let* ((image '(:type image :path "missing.png" :caption "😀"))
         (before (copy-tree image))
         (plan (gdocs-convert--image-to-requests image 8)))
    (should (equal plan '(:requests nil :index 8)))
    (should (equal image before))))

(ert-deftest gdocs-image-convert-authored-identity-over-transient-urls ()
  "Authored source/caption identity ignores remote IDs but detects authored edits."
  (let* ((source '(:type image :path "img/a.png" :caption "😀"))
         (remote '(:type image :path "img/a.png" :caption "😀" :object-id "remote"
                         :content-uri "https://tmp.invalid/one" :source-uri "https://images.invalid/two")))
    (should-not (gdocs-diff-generate (list remote) (list source)))
    (should (gdocs-diff-generate (list remote) (list (plist-put (copy-sequence source) :path "img/b.png"))))
    (should (= (gdocs-diff--element-utf16-length source) 1))))

(provide 'gdocs-image-convert-test)
;;; gdocs-image-convert-test.el ends here
