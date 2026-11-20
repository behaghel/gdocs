;;; gdocs-convert-section-breaks.el --- Section break conversion -*- lexical-binding: t; -*-

;;; Commentary:
;; A section break is semantic; its configurable border is presentation.
;; Native horizontal-rule objects are readable but have no insertion request.
;; Published breaks use a marked blank paragraph and an unbordered separator.

;;; Code:

(require 'cl-lib)
(require 'org-element)
(require 'seq)
(require 'gdocs-convert-paragraph-codec)

(declare-function gdocs-convert--next-id "gdocs-convert" ())
(declare-function gdocs-convert--range-marker "gdocs-convert" (markers type start end))
(declare-function gdocs-convert--logical-style-paragraph-request "gdocs-style-resolve" (style-name start end))
(declare-function gdocs-convert--logical-style-text-request "gdocs-style-resolve" (style-name start end &optional segment-id))

(defun gdocs-convert-section-break-element-p (element)
  "Return non-nil when ELEMENT is a semantic section break."
  (eq (plist-get element :type) 'horizontal-rule))

(defun gdocs-convert-section-break-source (element)
  "Convert Org horizontal-rule ELEMENT into independently identified IR."
  (let ((parent (org-element-parent element)))
    (while parent
      (unless (memq (org-element-type parent) '(org-data section headline))
        (user-error "Section breaks inside %s are not supported; move the break outside" (org-element-type parent)))
      (setq parent (org-element-parent parent))))
  (list :type 'horizontal-rule :id (gdocs-convert--next-id)))

(defun gdocs-convert-section-break-to-org (_element)
  "Render section-break _ELEMENT in canonical Org syntax."
  "-----")

(defun gdocs-convert--horizontal-rule-to-requests (index &optional element)
  "Publish a section break at INDEX, optionally using ELEMENT's identity."
  (let* ((element (or element (list :type 'horizontal-rule :id (gdocs-convert--next-id))))
         (current (1+ index))
         (requests (list (gdocs-convert--make-insert-text-request "\n" index)
                         (gdocs-convert--logical-style-paragraph-request 'gdocs-section-break index (1+ current))
                         (gdocs-convert--logical-style-text-request 'gdocs-section-break index current)
                         `((createNamedRange . ((name . ,(format "gdocs-org-marker:section-break:%s:v1" (plist-get element :id)))
                                                (range . ((startIndex . ,index) (endIndex . ,current))))))))
         (plan (gdocs-convert--append-generated-separator-requests (delq nil requests) current element))
         (end (plist-get plan :index)))
    ;; Full newline ranges style empty paragraphs too; the resolver's normal
    ;; text-span convention excludes the terminal newline.
    (plist-put plan :requests
               (append (plist-get plan :requests)
                       (delq nil (list (gdocs-convert--logical-style-paragraph-request 'gdocs-section-break-separator current (1+ end))
                                       (gdocs-convert--logical-style-text-request 'gdocs-section-break-separator current end)))))
    plan))

(defun gdocs-convert-section-break-validate-native-marker (type ranges)
  "Require section-break TYPE RANGES to identify exactly one newline."
  (when (eq type 'section-break)
    (unless (and (or (listp ranges) (vectorp ranges)) (= (length ranges) 1)
                 (let ((range (elt ranges 0)))
                   (and (integerp (alist-get 'startIndex range)) (integerp (alist-get 'endIndex range))
                        (= (1+ (alist-get 'startIndex range)) (alist-get 'endIndex range))
                        (not (alist-get 'segmentId range)))))
      (user-error "Section break provenance requires one complete body newline"))))

(defun gdocs-convert-section-break--blank-paragraph-p (paragraph)
  "Return non-nil for a text-only, unlisted blank PARAGRAPH."
  (and (not (alist-get 'bullet paragraph))
       (seq-every-p (lambda (part) (alist-get 'textRun part)) (alist-get 'elements paragraph))
       (equal (mapconcat (lambda (part) (or (alist-get 'content (alist-get 'textRun part)) ""))
                         (alist-get 'elements paragraph) "") "\n")))

(defun gdocs-convert-section-break-validate-native-context (content markers)
  "Validate marked break CONTENT against authoritative MARKERS, not borders."
  (dolist (marker markers)
    (when (eq (plist-get marker :type) 'section-break)
      (let* ((start (plist-get marker :start)) (end (plist-get marker :end))
             (element (seq-find (lambda (entry) (and (equal (alist-get 'startIndex entry) start)
                                                     (equal (alist-get 'endIndex entry) end))) content)))
        (unless (equal (plist-get marker :data) "v1")
          (user-error "Section break provenance is missing or unsupported; republish authoritative Org"))
        (unless (and element (alist-get 'paragraph element)
                     (gdocs-convert-section-break--blank-paragraph-p (alist-get 'paragraph element)))
          (user-error "Section break annotations must cover a whole blank, unlisted paragraph; move authored text outside the separator"))
        (when (seq-some (lambda (other)
                          (and (not (eq other marker)) (integerp (plist-get other :start)) (integerp (plist-get other :end))
                               (< (plist-get other :start) end) (> (plist-get other :end) start))) markers)
          (user-error "Section breaks cannot overlap other semantic annotations"))))))

(defun gdocs-convert-section-break-validate-native-table (table)
  "Reject native horizontal rules inside unsupported TABLE cell prose."
  (cl-labels ((contains-rule (node)
                (cond ((consp node) (or (eq (car node) 'horizontalRule)
                                        (contains-rule (car node)) (contains-rule (cdr node))))
                      ((vectorp node) (seq-some #'contains-rule node)))))
             (when (contains-rule table)
               (user-error "Native horizontal rules inside table cells are not supported; move the break outside the table"))))

(defun gdocs-convert-section-break-paragraph-from-docs (paragraph markers start end)
  "Recognize section-break PARAGRAPH at START..END using MARKERS or native type."
  (let ((marker (gdocs-convert--range-marker markers 'section-break start end))
        (rules (seq-filter (lambda (part) (assq 'horizontalRule part)) (alist-get 'elements paragraph))))
    (cond
     (marker (list :type 'horizontal-rule :id (gdocs-convert--next-id) :section-break-source 'marked))
     (rules
      (unless (and (= (length rules) 1) (not (alist-get 'bullet paragraph))
                   (integerp start) (integerp end) (= (- end start) 2)
                   (seq-every-p (lambda (part) (or (assq 'horizontalRule part)
                                                   (equal (alist-get 'content (alist-get 'textRun part)) "\n")))
                                (alist-get 'elements paragraph)))
        (user-error "A native horizontal rule must occupy its own unlisted paragraph"))
      (list :type 'horizontal-rule :id (gdocs-convert--next-id) :section-break-source 'native)))))

(defun gdocs-convert-section-breaks-finalize-native (ir)
  "Consume only publisher-owned outside separators after marked breaks in IR."
  (let ((remaining ir) result)
    (while remaining
      (let ((element (pop remaining)))
        (when (and (gdocs-convert-section-break-element-p element)
                   (eq (plist-get element :section-break-source) 'marked)
                   remaining (not (plist-get (car remaining) :gdocs-marker)))
          (let ((separator (gdocs-convert--consume-generated-separator remaining (plist-get element :doc-end))))
            (setq remaining (plist-get separator :remaining))
            (plist-put element :doc-end (plist-get separator :end))))
        (push element result)))
    (nreverse result)))

(provide 'gdocs-convert-section-breaks)
;;; gdocs-convert-section-breaks.el ends here
