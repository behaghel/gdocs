;;; gdocs-convert-inline-codec.el --- Shared rich inline conversion -*- lexical-binding: t; -*-

;;; Commentary:
;; Rich text, emphasis, literal links and normalization compose feature-owned
;; object adapters and run policies.  The facade supplies document link lookup,
;; semantic dispatch and policy composition; no feature registry is used here.

;;; Code:

(require 'cl-lib)
(require 'org-element)
(require 'seq)
(require 'subr-x)
(require 'gdocs-convert-text)

(declare-function gdocs-convert--typed-object-to-runs "gdocs-convert" (object inherited-props))
(declare-function gdocs-convert--inline-element-parts "gdocs-convert" (element markers))
(declare-function gdocs-convert--inline-finalize-runs "gdocs-convert" (runs))
(declare-function gdocs-convert--inline-run-policy "gdocs-convert" (run))
(declare-function gdocs-convert--run-to-org "gdocs-convert" (run))
(declare-function gdocs-convert--link-url "gdocs-link-resolve" (link))
(declare-function gdocs-convert--inline-link-for-org "gdocs-link-resolve" (link))
(declare-function gdocs-convert--extract-link-url "gdocs-link-resolve" (link-obj))
(declare-function gdocs-convert--docs-is-monospace-p "gdocs-convert-org-blocks" (style))

(defun gdocs-convert--objects-to-runs (objects &optional inherited-props)
  "Convert Org OBJECTS to merged runs with optional INHERITED-PROPS."
  (let (runs)
    (dolist (object (if (listp objects) objects (list objects)))
      (setq runs (nconc runs (gdocs-convert--object-to-runs object inherited-props))))
    (gdocs-convert--merge-adjacent-runs runs)))

(defun gdocs-convert--object-to-runs (object inherited-props)
  "Decode OBJECT with INHERITED-PROPS and preserve its post-blank spacing."
  (if (stringp object) (gdocs-convert--string-to-run object inherited-props)
    (let ((runs (gdocs-convert--typed-object-to-runs object inherited-props))
          (post-blank (or (org-element-property :post-blank object) 0)))
      (when (> post-blank 0)
        (setq runs (append runs (list (gdocs-convert--make-run
                                       (make-string post-blank ?\s) inherited-props)))))
      runs)))

(defun gdocs-convert-inline-plain-object (object inherited-props)
  "Decode ordinary rich-text OBJECT with INHERITED-PROPS after feature dispatch."
  (pcase (org-element-type object)
    ('bold (gdocs-convert--markup-to-runs object inherited-props :bold))
    ('italic (gdocs-convert--markup-to-runs object inherited-props :italic))
    ('underline (gdocs-convert--markup-to-runs object inherited-props :underline))
    ('strike-through (gdocs-convert--markup-to-runs object inherited-props :strikethrough))
    ('link (gdocs-convert--link-to-runs object inherited-props))
    ('subscript (gdocs-convert--markup-to-runs object inherited-props :subscript))
    ('superscript (gdocs-convert--markup-to-runs object inherited-props :superscript))
    ('entity (gdocs-convert--entity-to-run object inherited-props))
    ('line-break (list (gdocs-convert--make-run "\n" inherited-props)))
    (_ (gdocs-convert--fallback-object-to-runs object inherited-props))))

(defun gdocs-convert--markup-to-runs (object inherited-props formatting-key)
  "Decode markup OBJECT with INHERITED-PROPS and FORMATTING-KEY."
  (let ((props (plist-put (copy-sequence inherited-props) formatting-key t)) runs)
    (dolist (child (org-element-contents object))
      (setq runs (nconc runs (gdocs-convert--object-to-runs child props))))
    runs))

(defun gdocs-convert--link-to-runs (link inherited-props)
  "Decode ordinary LINK description with INHERITED-PROPS and document lookup."
  (let* ((url (gdocs-convert--link-url link))
         (children (org-element-contents link))
         (props (if url (plist-put (copy-sequence inherited-props) :link url) inherited-props)))
    (if children
        (let (runs)
          (dolist (child children)
            (setq runs (nconc runs (gdocs-convert--object-to-runs child props))))
          (or runs (list (gdocs-convert--make-run (or url (org-element-property :raw-link link)) props))))
      (list (gdocs-convert--make-run (or url (org-element-property :raw-link link)) props)))))

(defun gdocs-convert--entity-to-run (entity inherited-props)
  "Convert ENTITY to UTF-8 text with INHERITED-PROPS."
  (list (gdocs-convert--make-run (or (org-element-property :utf-8 entity) "") inherited-props)))

(defun gdocs-convert--fallback-object-to-runs (object inherited-props)
  "Interpret unknown OBJECT as text with INHERITED-PROPS."
  (let ((text (org-element-interpret-data object)))
    (when (and text (not (string-empty-p text)))
      (list (gdocs-convert--make-run text inherited-props)))))

(defun gdocs-convert--runs-to-org (runs)
  "Render RUNS through composed inline rendering."
  (mapconcat #'gdocs-convert--run-to-org runs ""))

(defun gdocs-convert-inline-plain-run-to-org (run)
  "Render ordinary rich-text RUN after semantic rendering has declined it."
  (let ((text (plist-get run :text)) (link (plist-get run :link)))
    (when (plist-get run :code) (setq text (format "~%s~" text)))
    (when link (setq text (format "[[%s][%s]]" (gdocs-convert--inline-link-for-org link) text)))
    (dolist (formatting '((:bold . "*") (:italic . "/") (:underline . "_") (:strikethrough . "+")))
      (when (plist-get run (car formatting))
        (setq text (gdocs-convert--wrap-emphasis (cdr formatting) text))))
    (when (plist-get run :subscript) (setq text (format "_{%s}" text)))
    (when (plist-get run :superscript) (setq text (format "^{%s}" text)))
    text))

(defun gdocs-convert--wrap-emphasis (marker text)
  "Wrap TEXT with MARKER while preserving whitespace outside emphasis."
  (if (string-blank-p text) text
    (let ((leading "") (trailing "") (inner text))
      (when (string-match "\\`\\([ \t\n]+\\)" inner)
        (setq leading (match-string 1 inner) inner (substring inner (match-end 1))))
      (when (and (not (string-empty-p inner))
                 (string-match "\\([ \t\n]+\\)\\'" inner))
        (setq trailing (match-string 1 inner) inner (substring inner 0 (match-beginning 1))))
      (if (string-empty-p inner) text (concat leading marker inner marker trailing)))))

(defun gdocs-convert--docs-elements-to-runs (elements &optional markers literal-whitespace)
  "Decode native ELEMENTS using MARKERS and feature-owned part dispatch.
LITERAL-WHITESPACE suppresses normalization of authored edge spacing."
  (let (runs previous-marker)
    (seq-doseq (element elements)
      (let ((parts (gdocs-convert--inline-element-parts element markers)))
        (unless parts
          (when-let* ((unknown (seq-remove (lambda (field)
                                             (memq (car field) '(startIndex endIndex suggestedInsertionIds suggestedDeletionIds))) element)))
            (user-error "Unsupported native inline object (%s); preserve the document and review its source representation"
                        (mapconcat (lambda (field) (symbol-name (car field))) unknown ", "))))
        (dolist (part parts)
          (let ((marker (car part)) (run (cdr part)))
            (if (and marker (eq marker previous-marker))
                (plist-put (car runs) :text (concat (plist-get (car runs) :text) (plist-get run :text)))
              (push run runs))
            (setq previous-marker marker)))))
    (let ((decoded (gdocs-convert--merge-adjacent-runs
                    (gdocs-convert--inline-finalize-runs (nreverse runs)))))
      (if literal-whitespace decoded (gdocs-convert--normalize-run-whitespace decoded)))))

(defun gdocs-convert--merge-adjacent-runs (runs)
  "Merge adjacent RUNS with matching formatting and compatible run policies."
  (when runs
    (let ((result (list (car runs))))
      (dolist (run (cdr runs))
        (let ((previous (car result)))
          (if (gdocs-convert--runs-same-format-p previous run)
              (setcar result (plist-put (copy-sequence previous) :text
                                        (concat (plist-get previous :text) (plist-get run :text))))
            (push run result))))
      (nreverse result))))

(defun gdocs-convert--runs-same-format-p (a b)
  "Return non-nil when runs A and B may coalesce without losing identity."
  (let ((pa (gdocs-convert--inline-run-policy a)) (pb (gdocs-convert--inline-run-policy b)))
    (and (not (or (plist-get pa :atomic) (plist-get pb :atomic)))
         (cl-every (lambda (key) (eq (plist-get a key) (plist-get b key)))
                   '(:bold :italic :underline :strikethrough :code :subscript :superscript))
         (cl-every (lambda (key) (equal (plist-get a key) (plist-get b key)))
                   '(:link :foreground-color :background-color))
         (eql (plist-get a :font-size) (plist-get b :font-size))
         (equal (plist-get pa :identity) (plist-get pb :identity)))))

(defun gdocs-convert--normalize-run-whitespace (runs)
  "Move formatted RUNS' edge whitespace into plain runs unless policy preserves it."
  (let (result)
    (dolist (run runs)
      (cond
       ((or (plist-get (gdocs-convert--inline-run-policy run) :preserve-whitespace)
            (not (gdocs-convert--run-has-formatting-p run))) (push run result))
       (t
        (let ((text (plist-get run :text)) (leading "") (trailing ""))
          (when (string-match "\\`\\([ \t]+\\)" text)
            (setq leading (match-string 1 text) text (substring text (match-end 1))))
          (when (and (not (string-empty-p text)) (string-match "\\([ \t]+\\)\\'" text))
            (setq trailing (match-string 1 text) text (substring text 0 (match-beginning 1))))
          (unless (string-empty-p leading) (push (gdocs-convert--make-plain-run leading) result))
          (unless (string-empty-p text) (push (plist-put (copy-sequence run) :text text) result))
          (unless (string-empty-p trailing) (push (gdocs-convert--make-plain-run trailing) result))))))
    (nreverse result)))

(defun gdocs-convert--run-has-formatting-p (run)
  "Return non-nil when RUN carries rich formatting or a policy-provided format."
  (or (seq-some (lambda (key) (plist-get run key))
                '(:bold :italic :underline :strikethrough :code :link :subscript :superscript
                        :foreground-color :background-color :font-size))
      (plist-get (gdocs-convert--inline-run-policy run) :formatted)))

(defun gdocs-convert--trim-trailing-newline-runs (runs)
  "Remove one native paragraph terminator from RUNS without trimming authored text."
  (when runs
    (let* ((result (copy-sequence runs))
           (tail (last result)) (run (car tail)) (text (or (plist-get run :text) "")))
      (when (string-suffix-p "\n" text)
        (let ((trimmed (substring text 0 -1)))
          (if (and (string-empty-p trimmed)
                   (not (plist-get (gdocs-convert--inline-run-policy run) :preserve-terminal)))
              (setq result (butlast result))
            (setcar tail (plist-put (copy-sequence run) :text trimmed)))))
      result)))

(defun gdocs-convert--docs-text-run-to-ir (text-run)
  "Convert native TEXT-RUN's authored formatting to a rich run."
  (let* ((content (gdocs-convert--strip-vertical-tabs (alist-get 'content text-run)))
         (style (alist-get 'textStyle text-run))
         (link-obj (alist-get 'link style))
         (url (when link-obj (gdocs-convert--extract-link-url link-obj)))
         (baseline (alist-get 'baselineOffset style))
         (fg (gdocs-convert--extract-color (alist-get 'foregroundColor style)))
         (bg (gdocs-convert--extract-color (alist-get 'backgroundColor style)))
         (size (gdocs-convert--extract-font-size (alist-get 'fontSize style)))
         (run (list :text content :bold (eq (alist-get 'bold style) t)
                    :italic (eq (alist-get 'italic style) t)
                    :underline (and (eq (alist-get 'underline style) t) (not url))
                    :strikethrough (eq (alist-get 'strikethrough style) t)
                    :code (gdocs-convert--docs-is-monospace-p style) :link url)))
    (when (plist-get run :code) (setq run (plist-put run :style-names '(gdocs-code))))
    (when (equal baseline "SUPERSCRIPT") (setq run (plist-put run :superscript t)))
    (when (equal baseline "SUBSCRIPT") (setq run (plist-put run :subscript t)))
    (when fg (setq run (plist-put run :foreground-color fg)))
    (when bg (setq run (plist-put run :background-color bg)))
    (when size (setq run (plist-put run :font-size size)))
    run))

(defun gdocs-convert--extract-color (color-obj)
  "Extract non-default semantic colour from native COLOR-OBJ."
  (when color-obj
    (let ((rgb (alist-get 'rgbColor (alist-get 'color color-obj))))
      (when rgb
        (let ((r (or (alist-get 'red rgb) 0)) (g (or (alist-get 'green rgb) 0))
              (b (or (alist-get 'blue rgb) 0)))
          (unless (and (= r 0) (= g 0) (= b 0))
            (format "#%02X%02X%02X" (round (* r 255)) (round (* g 255)) (round (* b 255)))))))))

(defun gdocs-convert--extract-font-size (font-size-obj)
  "Extract authored font size from native FONT-SIZE-OBJ."
  (when font-size-obj (alist-get 'magnitude font-size-obj)))

(provide 'gdocs-convert-inline-codec)
;;; gdocs-convert-inline-codec.el ends here
