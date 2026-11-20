;;; gdocs-convert-verse.el --- Lossless rich verse lines -*- lexical-binding: t; -*-

;;; Commentary:
;; Verse owns hard lines, literal spacing, versioned native groups and rendering.
;; Shared codecs supply rich runs without interpreting the enclosing feature.

;;; Code:

(require 'cl-lib)
(require 'org-element)
(require 'seq)
(require 'gdocs-convert-paragraph-codec)

(declare-function gdocs-convert--next-id "gdocs-convert" ())
(declare-function gdocs-convert--paragraph-to-requests "gdocs-convert" (element index))
(declare-function gdocs-convert--range-marker "gdocs-convert" (markers type start end))

(defun gdocs-convert-verse-element-p (element)
  "Return non-nil for semantic verse ELEMENT."
  (eq (plist-get element :type) 'verse-block))

(defun gdocs-convert-verse--split-runs (runs)
  "Split rich RUNS at literal newlines, retaining empty lines and spaces."
  (let (lines line)
    (dolist (run runs)
      (let ((parts (split-string (or (plist-get run :text) "") "\n" nil)) (first t))
        (dolist (part parts)
          (unless first (push (nreverse line) lines) (setq line nil))
          (setq first nil)
          (unless (string-empty-p part)
            (let ((copy (copy-sequence run)))
              (plist-put copy :text part)
              (push copy line))))))
    (nreverse (cons (nreverse line) lines))))

(defun gdocs-convert-verse--validate-runs (runs)
  "Reject semantic RUNS that cannot be preserved in a verse line."
  (when (seq-some (lambda (run)
                    (or (plist-get run :date-element) (plist-get run :footnote-label)
                        (plist-get run :status) (plist-get run :inline-source))) runs)
    (user-error "Verse currently supports ordinary rich text, not dates, notes, status or inline source")))

(defun gdocs-convert--verse-block-to-ir (verse-block)
  "Convert Org VERSE-BLOCK to rich hard lines without prose normalization."
  (let ((parent (org-element-parent verse-block)))
    (while parent
      (unless (memq (org-element-type parent) '(org-data section headline))
        (user-error "Verse inside %s is not supported; move it to the document body" (org-element-type parent)))
      (setq parent (org-element-parent parent))))
  (let* ((runs (gdocs-convert--objects-to-runs (org-element-contents verse-block) '(:literal-whitespace t)))
         (contents (gdocs-convert--trim-trailing-newline-runs runs)))
    (gdocs-convert-verse--validate-runs contents)
    (list :type 'verse-block :lines (when runs (gdocs-convert-verse--split-runs contents))
          :id (gdocs-convert--next-id))))

(defun gdocs-convert--ir-verse-block-to-org (element)
  "Render verse ELEMENT with literal line boundaries and authored formatting."
  (let* ((lines (plist-get element :lines))
         (text (mapconcat #'gdocs-convert--runs-to-org lines "\n")))
    (concat "#+BEGIN_VERSE\n" text (when lines "\n") "#+END_VERSE")))

(defun gdocs-convert--verse-block-to-requests (element index)
  "Publish rich verse ELEMENT at INDEX with a whole-line native marker."
  (let* ((lines (or (plist-get element :lines) (list nil)))
         (count (length lines)) (ordinal 0) (current index) requests)
    (dolist (runs lines)
      (gdocs-convert-verse--validate-runs runs)
      (let* ((paragraph (gdocs-convert-make-paragraph runs :style 'normal
                                                      :style-name (gdocs-convert--block-line-style-name 'gdocs-verse ordinal count)
                                                      :id (plist-get element :id)))
             (plan (gdocs-convert--paragraph-to-requests paragraph current)))
        (setq requests (nconc requests (plist-get plan :requests)) current (plist-get plan :index))
        (cl-incf ordinal)))
    ;; Include terminal newlines: otherwise blank stanza paragraphs have no anchor.
    (setq requests
          (nconc requests (list `((createNamedRange
                                   . ((name . ,(format "gdocs-org-marker:verse-block:%s:%s" (plist-get element :id)
                                                       (if (plist-get element :lines) "v1" "v1-empty")))
                                      (range . ((startIndex . ,index) (endIndex . ,current)))))))))
    (gdocs-convert--append-generated-separator-requests requests current element)))

(defun gdocs-convert-verse-validate-native-marker (type ranges entry)
  "Validate verse TYPE RANGES in native named-range ENTRY before decoding."
  (when (eq type 'verse-block)
    (unless (and (or (listp ranges) (vectorp ranges)) (= (length ranges) 1))
      (user-error "Verse annotations require one contiguous native range"))
    (let ((range (elt ranges 0)))
      (unless (and (listp entry) (integerp (alist-get 'startIndex range))
                   (integerp (alist-get 'endIndex range))
                   (< (alist-get 'startIndex range) (alist-get 'endIndex range))
                   (not (alist-get 'segmentId range)))
        (user-error "Verse annotations require a nonempty body range")))))

(defun gdocs-convert-verse-validate-native-context (content markers)
  "Require verse MARKERS to identify complete text-only lines in CONTENT."
  (dolist (marker markers)
    (when (eq (plist-get marker :type) 'verse-block)
      (let* ((start (plist-get marker :start)) (end (plist-get marker :end))
             (elements (seq-filter
                        (lambda (element)
                          (and (integerp (alist-get 'startIndex element)) (integerp (alist-get 'endIndex element))
                               (< (alist-get 'startIndex element) end) (> (alist-get 'endIndex element) start))) content)))
        (unless (member (plist-get marker :data) '("v1" "v1-empty"))
          (user-error "Verse structure provenance is missing or unsupported; republish authoritative Org before pulling"))
        (unless (and elements (= (alist-get 'startIndex (car elements)) start)
                     (= (alist-get 'endIndex (car (last elements))) end)
                     (seq-every-p (lambda (element)
                                    (let ((paragraph (alist-get 'paragraph element)))
                                      (and paragraph (not (alist-get 'bullet paragraph))
                                           (seq-every-p (lambda (part) (alist-get 'textRun part))
                                                        (alist-get 'elements paragraph))))) elements))
          (user-error "Verse annotations must cover whole text-only, unlisted native lines"))
        (when (seq-some (lambda (other)
                          (and (not (eq other marker)) (integerp (plist-get other :start))
                               (integerp (plist-get other :end)) (< (plist-get other :start) end)
                               (> (plist-get other :end) start))) markers)
          (user-error "Verse cannot preserve overlapping semantic annotations"))))))

(defun gdocs-convert-verse-paragraph-from-docs (paragraph markers start end)
  "Decode literal PARAGRAPH at START..END when identified by verse MARKERS."
  (when-let* ((marker (gdocs-convert--range-marker markers 'verse-block start end)))
    (unless (member (plist-get marker :data) '("v1" "v1-empty"))
      (user-error "Verse structure provenance is missing or unsupported; republish authoritative Org before pulling"))
    (unless (and (integerp start) (integerp end)
                 (<= (plist-get marker :start) start) (>= (plist-get marker :end) end)
                 (not (alist-get 'bullet paragraph))
                 (seq-every-p (lambda (part) (alist-get 'textRun part)) (alist-get 'elements paragraph)))
      (user-error "Verse annotations must cover whole text-only, unlisted native lines"))
    (let ((runs (gdocs-convert-paragraph-docs-runs paragraph markers t)))
      (gdocs-convert-verse--validate-runs runs)
      (gdocs-convert-paragraph-from-docs paragraph runs markers start end 'normal nil))))

(defun gdocs-convert-verse--marker (element)
  "Return ELEMENT's structural verse marker, or nil."
  (seq-find (lambda (marker) (eq (plist-get marker :type) 'verse-block)) (plist-get element :gdocs-marker)))

(defun gdocs-convert--attach-verse-block-markers (ir)
  "Group whole native verse lines in IR by authoritative block identity."
  (let ((remaining ir) result)
    (while remaining
      (let* ((element (car remaining)) (marker (gdocs-convert-verse--marker element)))
        (if (not marker) (push (pop remaining) result)
          (let ((id (plist-get marker :id)) (start (plist-get element :doc-start)) end lines)
            (while (and remaining (gdocs-convert-verse--marker (car remaining))
                        (equal id (plist-get (gdocs-convert-verse--marker (car remaining)) :id)))
              (let ((paragraph (pop remaining)))
                (setq lines (append lines (gdocs-convert-verse--split-runs (plist-get paragraph :contents)))
                      end (plist-get paragraph :doc-end))))
            (unless (and (= start (plist-get marker :start)) (= end (plist-get marker :end)))
              (user-error "Verse group boundaries are incomplete; restore its native annotation or republish Org"))
            (when (and remaining (not (plist-get (car remaining) :gdocs-marker)))
              (let ((separator (gdocs-convert--consume-generated-separator remaining end)))
                (setq remaining (plist-get separator :remaining) end (plist-get separator :end))))
            (push (list :type 'verse-block
                        :lines (unless (and (equal (plist-get marker :data) "v1-empty") (equal lines (list nil))) lines)
                        :id (gdocs-convert--next-id) :doc-start start :doc-end end) result)))))
    (nreverse result)))

(provide 'gdocs-convert-verse)
;;; gdocs-convert-verse.el ends here
