;;; gdocs-style-resolve-test.el --- Presentation resolution boundaries -*- lexical-binding: t; -*-

;;; Commentary:
;; Resolve presentation without loading semantic conversion or changing identity.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'gdocs-convert)
(require 'gdocs-diff)

(defconst gdocs-style-resolve-test--fixtures
  (expand-file-name "fixtures/" (file-name-directory (or load-file-name buffer-file-name)))
  "Public specimen directory.")

(defconst gdocs-style-resolve-test--profile
  '((gdocs-body :parent normal :text (:font-family "Specimen Serif" :font-size 13)
                :paragraph (:space-above 0 :space-below 4 :indent-first-line 0
                                         :alignment justified
                                         :borders ((left . (:color "#123456" :width 2 :padding 3)))))
    (gdocs-heading-1 :parent heading-1 :text (:font-scale 1.6 :font-weight 600)
                     :paragraph (:space-above 12))
    (gdocs-code :text (:monospace t :foreground-color "#112233")))
  "Synthetic configurable presentation, not a personal publishing profile.")

(defun gdocs-style-resolve-test--fingerprints ()
  "Return IR, rendered Org and request fingerprints under the synthetic profile."
  (let* ((gdocs-style-definitions (append gdocs-style-resolve-test--profile gdocs-style-definitions))
         (source (with-temp-buffer
                   (insert-file-contents (expand-file-name "kitchen-sink.org" gdocs-style-resolve-test--fixtures))
                   (buffer-string)))
         (json-object-type 'alist) (json-array-type 'vector) (json-key-type 'symbol)
         (native (with-temp-buffer
                   (insert-file-contents (expand-file-name "kitchen-sink.json" gdocs-style-resolve-test--fixtures))
                   (goto-char (point-min)) (json-read)))
         (org-ir (gdocs-convert-org-string-to-ir source))
         (docs-ir (gdocs-convert-docs-json-to-ir native)))
    (mapcar (lambda (value) (secure-hash 'sha256 (prin1-to-string value)))
            (list org-ir docs-ir (gdocs-convert-ir-to-org org-ir) (gdocs-convert-ir-to-org docs-ir)
                  (gdocs-convert-ir-to-docs-requests org-ir)
                  (gdocs-convert-ir-to-docs-requests docs-ir)))))

(ert-deftest gdocs-style-resolve-loads-and-publishes-without-conversion ()
  "Logical styles resolve standalone, including named parents and segment text."
  (let ((directory (file-name-directory (locate-library "gdocs-convert"))))
    (with-temp-buffer
      (let* ((program
              '(progn
                 (require 'gdocs-style-resolve)
                 (dolist (feature '(gdocs-convert org gdocs-diff gdocs-convert-headings
                                                  gdocs-convert-status gdocs-convert-footnotes gdocs-api gdocs-auth))
                   (when (featurep feature) (error "Style resolver loaded %s" feature)))
                 (let* ((gdocs-style-base-font-size 10)
                        (gdocs-style-definitions
                         '((fixture :parent heading-2 :paragraph (:space-above 0 :indent-first-line 0)
                                    :text (:font-family "Specimen Serif" :font-scale 1.5 :bold :json-false))))
                        (requests (gdocs-convert-make-logical-style-requests 'fixture 5 17 7 14 "segment"))
                        (paragraph (alist-get 'updateParagraphStyle (car requests)))
                        (text (alist-get 'updateTextStyle (cadr requests))))
                   (unless (and (equal (alist-get 'namedStyleType (alist-get 'paragraphStyle paragraph)) "HEADING_2")
                                (= (alist-get 'endIndex (alist-get 'range paragraph)) 16)
                                (= (alist-get 'magnitude (alist-get 'spaceAbove (alist-get 'paragraphStyle paragraph))) 0)
                                (= (alist-get 'magnitude (alist-get 'fontSize (alist-get 'textStyle text))) 15.0)
                                (eq (alist-get 'bold (alist-get 'textStyle text)) :json-false)
                                (equal (alist-get 'range text) '((segmentId . "segment") (startIndex . 7) (endIndex . 14))))
                     (error "Standalone style request contract changed")))))
             (status (call-process (expand-file-name invocation-name invocation-directory) nil t nil
                                   "--batch" "-Q" "-L" directory "--eval" (prin1-to-string program))))
        (ert-info ((buffer-string)) (should (zerop status)))))))

(ert-deftest gdocs-style-resolve-owns-logical-style-translation ()
  "Translation, borders, colour encoding and logical request helpers share an owner."
  (dolist (function '(gdocs-convert--style-definition gdocs-convert--logical-text-style-to-docs
                                                      gdocs-convert--logical-paragraph-style-to-docs gdocs-convert--paragraph-border-object
                                                      gdocs-convert--logical-style-paragraph-request gdocs-convert--logical-style-text-request
                                                      gdocs-convert-make-logical-style-requests gdocs-convert--hex-to-color))
    (should (string-match-p "gdocs-style-resolve\\.elc?\\'" (symbol-file function 'defun)))))

(ert-deftest gdocs-style-resolve-preserves-explicit-zero-false-and-precedence ()
  "Explicit values and ordered overrides are not mistaken for absent configuration."
  (let ((gdocs-style-definitions '((fixture :text (:font-size 17)) (fixture :text (:font-size 29))))
        (gdocs-style-default-border-width 9) (gdocs-style-default-border-padding 8)
        (gdocs-style-base-font-size 10))
    (should (= (alist-get 'magnitude (alist-get 'fontSize (gdocs-convert--logical-style-text-fields 'fixture))) 17))
    (let ((border (gdocs-convert--paragraph-border-object '(:width 0 :padding 0) nil nil))
          (text (gdocs-convert--logical-text-style-to-docs '(:bold :json-false :italic nil :font-scale 2 :font-size 15))))
      (should (= (alist-get 'magnitude (alist-get 'width border)) 0))
      (should (= (alist-get 'magnitude (alist-get 'padding border)) 0))
      (should (eq (alist-get 'bold text) :json-false))
      (should (assq 'italic text))
      (should (= (alist-get 'magnitude (alist-get 'fontSize text)) 15)))
    (should-not (gdocs-convert-make-logical-style-requests 'undefined-style 1 4 1 3))))

(ert-deftest gdocs-style-resolve-keeps-style-configuration-data-only ()
  "Presentation configuration must not acquire executable conversion policy."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "gdocs-styles.el"
                                            (file-name-directory (locate-library "gdocs-convert"))))
    (goto-char (point-min))
    (condition-case nil
        (while t
          (let ((form (read (current-buffer))))
            (should (memq (car-safe form) '(require defgroup defcustom defvaralias provide)))
            (when (eq (car-safe form) 'require) (should (equal (cadr form) ''custom)))))
      (end-of-file nil))))

(ert-deftest gdocs-style-resolve-parent-enum-compatibility ()
  "Native named-style translation needs no semantic feature module."
  (dolist (entry '((normal . "NORMAL_TEXT") (nil . "NORMAL_TEXT")
                   (title . "TITLE") (subtitle . "SUBTITLE")
                   (heading-1 . "HEADING_1") (heading-6 . "HEADING_6")
                   (heading-7 . "NORMAL_TEXT") (unknown-role . "NORMAL_TEXT")))
    (should (equal (gdocs-convert--ir-style-to-docs (car entry)) (cdr entry))))
  (should (equal (gdocs-convert--ir-style-to-docs (make-symbol "heading-2")) "HEADING_2"))
  (should-error (gdocs-convert--ir-style-to-docs "heading-2") :type 'wrong-type-argument))

(ert-deftest gdocs-style-resolve-does-not-mutate-profile-data ()
  "Rendering style requests cannot modify caller-owned presentation data."
  (let* ((gdocs-style-definitions (copy-tree gdocs-style-resolve-test--profile))
         (before (copy-tree gdocs-style-definitions)))
    (dolist (name '(gdocs-body gdocs-heading-1 gdocs-code))
      (should (gdocs-convert-make-logical-style-requests name 3 18 3 17)))
    (should (equal before gdocs-style-definitions))))

(ert-deftest gdocs-style-resolve-preserves-empty-status-override ()
  "An explicitly empty status style disables fallback rather than disappearing."
  (let ((gdocs-style-definitions '((gdocs-status-green))))
    (should (gdocs-style-resolve-find-definition 'gdocs-status-green))
    (should-not (gdocs-convert--status-docs-style '(:color "green"))))
  (let ((gdocs-style-definitions nil))
    (should (gdocs-convert--status-docs-style '(:color "green")))))

(defun gdocs-style-resolve-test--contains (tree symbol)
  "Return non-nil when TREE contains SYMBOL."
  (if (consp tree)
      (or (gdocs-style-resolve-test--contains (car tree) symbol)
          (gdocs-style-resolve-test--contains (cdr tree) symbol))
    (eq tree symbol)))

(ert-deftest gdocs-style-resolve-facade-does-not-read-presentation-configuration ()
  "The converter cannot retain hidden theme lookup or fallback policy."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "gdocs-convert.el"
                                            (file-name-directory (locate-library "gdocs-convert"))))
    (goto-char (point-min))
    (let (forms)
      (condition-case nil (while t (push (read (current-buffer)) forms)) (end-of-file nil))
      (dolist (symbol '(gdocs-style-definitions gdocs-style-base-font-size
                                                gdocs-style-default-border-color gdocs-style-default-border-width
                                                gdocs-style-default-border-padding gdocs-convert-monospace-font-families))
        (should-not (gdocs-style-resolve-test--contains forms symbol))))))

(ert-deftest gdocs-style-resolve-small-caps-is-presentation-not-label-rewriting ()
  "Small caps resolve to native styling and can be explicitly disabled/reset."
  (should (eq (alist-get 'smallCaps (gdocs-convert--logical-text-style-to-docs '(:small-caps t))) t))
  (should (eq (alist-get 'smallCaps (gdocs-convert--logical-text-style-to-docs '(:small-caps nil))) :json-false))
  (let ((reset (alist-get 'updateTextStyle (gdocs-convert--make-text-style-reset-request 3 8))))
    (should (member "smallCaps" (split-string (alist-get 'fields reset) ",")))
    (should (eq (alist-get 'smallCaps (alist-get 'textStyle reset)) :json-false))))

(ert-deftest gdocs-style-resolve-explicit-empty-background-clears-inherited-shading ()
  "An outside paragraph clears shading without emitting empty wire objects."
  (let* ((gdocs-style-definitions '((outside :parent normal :paragraph (:background-color nil))))
         (request (alist-get 'updateParagraphStyle (gdocs-convert--logical-style-paragraph-request 'outside 5 6))))
    (should (member "shading.backgroundColor" (split-string (alist-get 'fields request) ",")))
    (should (equal (alist-get 'paragraphStyle request) '((namedStyleType . "NORMAL_TEXT"))))))

(provide 'gdocs-style-resolve-test)
;;; gdocs-style-resolve-test.el ends here
