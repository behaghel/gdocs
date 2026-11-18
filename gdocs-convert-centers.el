;;; gdocs-convert-centers.el --- Centered prose block conversion -*- lexical-binding: t; -*-

;;; Commentary:
;; Center owns source/native recognition, paragraph boundaries and publication.
;; Shared codecs and neutral separator helpers do not interpret center policy.

;;; Code:

(require 'cl-lib)
(require 'org-element)
(require 'seq)
(require 'gdocs-convert-paragraph-codec)

(declare-function gdocs-convert--next-id "gdocs-convert" ())
(declare-function gdocs-convert--trim-runs "gdocs-convert" (runs))
(declare-function gdocs-convert--paragraph-to-requests "gdocs-convert" (element index))
(declare-function gdocs-convert--range-marker "gdocs-convert" (markers type start end))
(declare-function gdocs-convert--standalone-image-link-p "gdocs-convert-images" (object))

(defun gdocs-convert-center-element-p (element)
  "Return non-nil when ELEMENT is a centered prose block."
  (eq (plist-get element :type) 'center-block))

(defun gdocs-convert-center--validate-runs (runs)
  "Reject semantic RUNS not preserved by centered prose publication."
  (when (seq-some (lambda (run) (or (plist-get run :date-element) (plist-get run :footnote-label)
                                    (plist-get run :status) (plist-get run :inline-source))) runs)
    (user-error "Centered blocks support ordinary rich text, not dates, notes, status or inline source")))

(defun gdocs-convert--center-block-to-ir (center-block)
  "Return centered block IR for Org CENTER-BLOCK, retaining paragraph identity."
  (let ((parent (org-element-parent center-block)) paragraphs)
    (while parent
      (unless (memq (org-element-type parent) '(org-data section headline))
        (user-error "Center inside %s is not supported; move it to the document body" (org-element-type parent)))
      (setq parent (org-element-parent parent)))
    (dolist (child (org-element-contents center-block))
      (unless (eq (org-element-type child) 'paragraph)
        (user-error "Unsupported %s inside centered prose; move it outside the block" (org-element-type child)))
      (org-element-map child '(latex-fragment export-snippet macro citation target radio-target)
                       (lambda (object) (user-error "Unsupported %s inside centered prose" (org-element-type object))))
      (when (org-element-map child 'link #'gdocs-convert--standalone-image-link-p nil t)
        (user-error "Images inside centered prose are not supported; move the image outside the block"))
      (let ((runs (gdocs-convert--trim-runs (gdocs-convert--objects-to-runs (org-element-contents child)))))
        (gdocs-convert-center--validate-runs runs)
        (when runs (push runs paragraphs))))
    (list (list :type 'center-block :paragraphs (nreverse paragraphs) :id (gdocs-convert--next-id)))))

(defun gdocs-convert--ir-center-block-to-org (element)
  "Render centered ELEMENT with its authored paragraph and block boundaries."
  (let ((paragraphs (plist-get element :paragraphs)))
    (concat "#+BEGIN_CENTER\n" (mapconcat #'gdocs-convert--runs-to-org paragraphs "\n\n")
            (when paragraphs "\n") "#+END_CENTER")))

(defun gdocs-convert-center-paragraph-to-org (element text)
  "Render unmarked centered paragraph ELEMENT and TEXT without guessing groups."
  (when (eq (plist-get element :alignment) 'center)
    (format "#+BEGIN_CENTER\n%s\n#+END_CENTER" text)))

(defun gdocs-convert--center-block-to-requests (element index)
  "Publish centered ELEMENT at INDEX with whole-paragraph structural provenance."
  (let* ((paragraphs (or (plist-get element :paragraphs) (list nil)))
         (count (length paragraphs)) (ordinal 0) (current index) requests)
    (dolist (runs paragraphs)
      (gdocs-convert-center--validate-runs runs)
      (let* ((paragraph (gdocs-convert-make-paragraph runs :style 'normal :alignment 'center
                                                      :style-name (gdocs-convert--block-line-style-name 'gdocs-center ordinal count)))
             (plan (gdocs-convert--paragraph-to-requests paragraph current)))
        (setq requests (nconc requests (plist-get plan :requests)) current (plist-get plan :index))
        (cl-incf ordinal)))
    (setq requests (nconc requests
                          (list `((createNamedRange . ((name . ,(format "gdocs-org-marker:center-block:%s:v1" (plist-get element :id)))
                                                       (range . ((startIndex . ,index) (endIndex . ,current)))))))))
    ;; Cover terminal newlines as well: empty structural placeholders have no text span.
    (setq requests (nconc requests (list `((updateParagraphStyle
                                            . ((paragraphStyle . ((alignment . "CENTER")))
                                               (range . ((startIndex . ,index) (endIndex . ,current)))
                                               (fields . "alignment")))))))
    (let ((plan (gdocs-convert--append-generated-separator-requests requests current element)))
      ;; The outside separator must not lend center alignment to following prose.
      (plist-put plan :requests
                 (append (plist-get plan :requests)
                         (list `((updateParagraphStyle
                                  . ((paragraphStyle . ((alignment . "START")))
                                     (range . ((startIndex . ,current) (endIndex . ,(plist-get plan :index))))
                                     (fields . "alignment")))))))
      plan)))

(defun gdocs-convert-center-validate-native-marker (type ranges)
  "Validate TYPE's native RANGES before generic marker decoding."
  (when (eq type 'center-block)
    (unless (and (or (listp ranges) (vectorp ranges)) (= (length ranges) 1)
                 (let ((range (elt ranges 0)))
                   (and (integerp (alist-get 'startIndex range)) (integerp (alist-get 'endIndex range))
                        (< (alist-get 'startIndex range) (alist-get 'endIndex range))
                        (not (alist-get 'segmentId range)))))
      (user-error "Centered block annotations require one contiguous, nonempty body range"))))

(defun gdocs-convert-center-validate-native-context (content markers)
  "Validate centered MARKERS against whole aligned paragraphs in CONTENT."
  (dolist (marker markers)
    (when (eq (plist-get marker :type) 'center-block)
      (let* ((start (plist-get marker :start)) (end (plist-get marker :end))
             (elements (seq-filter (lambda (element)
                                     (and (integerp (alist-get 'startIndex element)) (integerp (alist-get 'endIndex element))
                                          (< (alist-get 'startIndex element) end) (> (alist-get 'endIndex element) start))) content)))
        (unless (equal (plist-get marker :data) "v1")
          (user-error "Centered block provenance is missing or unsupported; republish authoritative Org"))
        (unless (and elements (= (alist-get 'startIndex (car elements)) start)
                     (= (alist-get 'endIndex (car (last elements))) end)
                     (seq-every-p (lambda (element)
                                    (let ((paragraph (alist-get 'paragraph element)))
                                      (and paragraph (not (alist-get 'bullet paragraph))
                                           (equal (alist-get 'alignment (alist-get 'paragraphStyle paragraph)) "CENTER")
                                           (seq-every-p (lambda (part) (alist-get 'textRun part)) (alist-get 'elements paragraph))))) elements))
          (user-error "Centered block annotations require whole centered text-only, unlisted paragraphs; restore alignment or republish Org"))
        (when (seq-some (lambda (other)
                          (and (not (eq other marker)) (integerp (plist-get other :start)) (integerp (plist-get other :end))
                               (< (plist-get other :start) end) (> (plist-get other :end) start))) markers)
          (user-error "Centered blocks cannot preserve overlapping semantic annotations"))))))

(defun gdocs-convert-center-paragraph-from-docs (paragraph markers start end)
  "Decode native PARAGRAPH at START..END when identified by centered MARKERS."
  (when (gdocs-convert--range-marker markers 'center-block start end)
    (let ((runs (gdocs-convert-paragraph-docs-runs paragraph markers)))
      (gdocs-convert-center--validate-runs runs)
      (gdocs-convert-paragraph-from-docs paragraph runs markers start end 'normal nil))))

(defun gdocs-convert-center--marker (element)
  "Return ELEMENT's structural centered marker, or nil."
  (seq-find (lambda (marker) (eq (plist-get marker :type) 'center-block)) (plist-get element :gdocs-marker)))

(defun gdocs-convert--attach-center-block-markers (ir)
  "Group native IR paragraphs using centered provenance, never adjacency alone."
  (let ((remaining ir) result)
    (while remaining
      (let* ((element (car remaining)) (marker (gdocs-convert-center--marker element)))
        (if (not marker) (push (pop remaining) result)
          (let ((id (plist-get marker :id)) (start (plist-get element :doc-start)) end paragraphs)
            (while (and remaining (gdocs-convert-center--marker (car remaining))
                        (equal id (plist-get (gdocs-convert-center--marker (car remaining)) :id)))
              (let ((paragraph (pop remaining)))
                (when (plist-get paragraph :contents) (push (plist-get paragraph :contents) paragraphs))
                (setq end (plist-get paragraph :doc-end))))
            (unless (and (= start (plist-get marker :start)) (= end (plist-get marker :end)))
              (user-error "Centered block boundaries are incomplete; restore provenance or republish Org"))
            (when (and remaining (not (plist-get (car remaining) :gdocs-marker)))
              (let ((separator (gdocs-convert--consume-generated-separator remaining end)))
                (setq remaining (plist-get separator :remaining) end (plist-get separator :end))))
            (push (list :type 'center-block :paragraphs (nreverse paragraphs)
                        :id (gdocs-convert--next-id) :doc-start start :doc-end end) result)))))
    (nreverse result)))

(provide 'gdocs-convert-centers)
;;; gdocs-convert-centers.el ends here
