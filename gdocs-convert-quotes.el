;;; gdocs-convert-quotes.el --- Quote conversion and request policy -*- lexical-binding: t; -*-

;;; Commentary:
;; Own source/context recognition, native marker grouping, visual cleanup,
;; rendering and request planning.  Shared text/paragraph services supply
;; construction, soft-wrap heuristics and neutral separator operations.

;;; Code:

(require 'cl-lib)
(require 'org-element)
(require 'seq)
(require 'gdocs-convert-paragraph-codec)

(declare-function gdocs-convert--next-id "gdocs-convert" ())
(declare-function gdocs-convert--trim-runs "gdocs-convert" (runs))
(declare-function gdocs-convert--paragraph-to-requests "gdocs-convert" (element index))
(declare-function gdocs-convert--run-model-length "gdocs-convert" (run))
(declare-function gdocs-convert--logical-style-text-fields "gdocs-style-resolve" (style-name))

(defun gdocs-convert-quote-element-p (element)
  "Return non-nil for semantic quote ELEMENT."
  (eq (plist-get element :type) 'quote-block))

(defun gdocs-convert--paragraph-style (paragraph)
  "Determine the IR style for PARAGRAPH based on its quote context."
  (let ((parent (org-element-parent paragraph)))
    (if (eq (org-element-type parent) 'quote-block) 'quote 'normal)))

(defun gdocs-convert-quote-paragraph-to-ir (paragraph runs)
  "Return contextual quote IR for PARAGRAPH and RUNS, or nil."
  (when (and runs (eq (gdocs-convert--paragraph-style paragraph) 'quote))
    (list (gdocs-convert-paragraph-from-runs runs 'quote))))

(defun gdocs-convert-quote-paragraph-to-org (element text)
  "Render contextual quote ELEMENT and TEXT, or nil when unhandled."
  (when (eq (plist-get element :style) 'quote)
    (format "#+BEGIN_QUOTE\n%s\n#+END_QUOTE" text)))

(defun gdocs-convert--block-paragraph-runs (block)
  "Return non-empty paragraph run lists from Org quote BLOCK."
  (let (paragraphs)
    (org-element-map block 'paragraph
                     (lambda (paragraph)
                       (let* ((runs (gdocs-convert--objects-to-runs (org-element-contents paragraph)))
                              (trimmed (gdocs-convert--trim-runs runs)))
                         (when trimmed (push trimmed paragraphs)))))
    (nreverse paragraphs)))

(defun gdocs-convert--quote-block-to-ir (quote-block)
  "Convert a QUOTE-BLOCK element to semantic quote-block IR."
  (let ((paragraphs (gdocs-convert--block-paragraph-runs quote-block)))
    (list :type 'quote-block
          :paragraphs (or paragraphs (list (list (gdocs-convert--make-plain-run ""))))
          :id (gdocs-convert--next-id))))

(defun gdocs-convert--quote-block-marker (element)
  "Return the quote-block marker on ELEMENT, or nil."
  (seq-find (lambda (marker) (eq (plist-get marker :type) 'quote-block))
            (plist-get element :gdocs-marker)))

(defun gdocs-convert--quote-block-paragraph-p (element)
  "Return non-nil if ELEMENT is a marked quote-block paragraph."
  (and (eq (plist-get element :type) 'paragraph) (gdocs-convert--quote-block-marker element)))

(defun gdocs-convert--ir-quote-block-to-org (element)
  "Convert quote-block IR ELEMENT to Org quote block text."
  (format "#+BEGIN_QUOTE\n%s\n#+END_QUOTE"
          (mapconcat #'gdocs-convert--runs-to-org (plist-get element :paragraphs) "\n\n")))

(defun gdocs-convert--strip-quote-visual-style (runs &optional marker)
  "Recover authored italics in RUNS from the applied style snapshot in MARKER.
Without MARKER, preserve native emphasis rather than guessing at decoration.
A marked quote without provenance must be republished from authoritative Org."
  (let* ((data (plist-get marker :data))
         (base (cond ((null marker) nil)
                     ((equal data "italic-v1-0") nil)
                     ((equal data "italic-v1-1") t)
                     (t (user-error "Quote style provenance is missing or unsupported; push from authoritative Org source before pulling this quote")))))
    ;; Native normalization happened in displayed-style space.  Normalize again
    ;; after inversion so edge spaces do not become spurious authored emphasis.
    (gdocs-convert--merge-adjacent-runs
     (gdocs-convert--normalize-run-whitespace
      (mapcar (lambda (run)
                (plist-put (copy-sequence run) :italic
                           (not (eq (eq (plist-get run :italic) t) base)))) runs)))))

(defun gdocs-convert--attach-quote-block-markers (ir)
  "Group consecutive marked quote-block paragraphs in IR."
  (let ((result nil) (remaining ir))
    (while remaining
      (let ((element (car remaining)))
        (if (gdocs-convert--quote-block-paragraph-p element)
            (let ((paragraphs nil)
                  (block-id (plist-get (gdocs-convert--quote-block-marker element) :id))
                  (start (plist-get element :doc-start)) end)
              (while (and remaining (gdocs-convert--quote-block-paragraph-p (car remaining))
                          (equal block-id (plist-get (gdocs-convert--quote-block-marker (car remaining)) :id)))
                (unless (gdocs-convert--empty-paragraph-p (car remaining))
                  (let* ((marker (gdocs-convert--quote-block-marker (car remaining)))
                         (runs (gdocs-convert--strip-quote-visual-style (plist-get (car remaining) :contents) marker)))
                    ;; Native paragraph breaks are authored structure, not a
                    ;; casing-based guess at visual line wrapping.
                    (push runs paragraphs)))
                (setq end (plist-get (car remaining) :doc-end) remaining (cdr remaining)))
              (let ((separator (gdocs-convert--consume-generated-separator remaining end)))
                (setq remaining (plist-get separator :remaining) end (plist-get separator :end)))
              (push (append (list :type 'quote-block :paragraphs (nreverse paragraphs) :id (gdocs-convert--next-id))
                            (when start (list :doc-start start)) (when end (list :doc-end end))) result))
          (push element result)
          (setq remaining (cdr remaining)))))
    (nreverse result)))

(defun gdocs-convert--quote-block-paragraph-element (element runs index count)
  "Return paragraph IR for quote-block ELEMENT RUNS at INDEX of COUNT."
  (gdocs-convert--typographic-block-paragraph
   element runs 'gdocs-quote-block (list :type 'quote-block) index count))

(defun gdocs-convert--quote-block-to-requests (element index)
  "Create paragraph requests for quote-block ELEMENT at INDEX."
  (let* ((paragraphs (or (plist-get element :paragraphs) (list (list (gdocs-convert--make-plain-run "")))))
         (count (length paragraphs)) (paragraph-index 0) (requests nil) (current index))
    (dolist (runs paragraphs)
      (let* ((paragraph (gdocs-convert--quote-block-paragraph-element element runs paragraph-index count))
             (base (eq (alist-get 'italic (gdocs-convert--logical-style-text-fields
                                           (plist-get paragraph :style-name))) t))
             (snapshot (if base "italic-v1-1" "italic-v1-0"))
             (_ (plist-put (car (plist-get paragraph :gdocs-marker)) :data snapshot))
             (result (gdocs-convert--paragraph-to-requests paragraph current))
             (position current))
        (setq requests (nconc requests (plist-get result :requests)))
        ;; Apply both true and false explicitly, after logical and authored
        ;; styles.  This makes displayed italic = authored XOR applied base.
        (dolist (run runs)
          (let* ((end (+ position (gdocs-convert--run-model-length run)))
                 (italic (not (eq (eq (plist-get run :italic) t) base))))
            (when (> end position)
              (setq requests (nconc requests
                                    (list `((updateTextStyle
                                             . ((textStyle . ((italic . ,(if italic t :json-false))))
                                                (range . ((startIndex . ,position) (endIndex . ,end)))
                                                (fields . "italic"))))))))
            (setq position end)))
        (setq current (plist-get result :index))
        (cl-incf paragraph-index)))
    (gdocs-convert--append-generated-separator-requests requests current element)))

(provide 'gdocs-convert-quotes)
;;; gdocs-convert-quotes.el ends here
