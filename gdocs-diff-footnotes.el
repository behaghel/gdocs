;;; gdocs-diff-footnotes.el --- Encapsulated native footnote diff policy -*- lexical-binding: t; -*-

;;; Commentary:
;; Own footnote identity, zero-width body contributions, inline fingerprints,
;; whitespace normalization, reference emission and native segment planning.
;; The generic engine consumes only neutral descriptors and request plans.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'gdocs-convert-footnotes)

(defvar gdocs-diff-semantic-element-functions)
(defvar gdocs-diff-semantic-run-functions)
(defvar gdocs-diff-semantic-plan-functions)
(defvar gdocs-convert-footnote-reference-text-function)
(defvar gdocs-convert-footnote-reference-handler)
(declare-function gdocs-diff--runs-formatting-key "gdocs-diff" (runs))
(declare-function gdocs-convert--runs-to-plain-text "gdocs-convert" (runs))

(defun gdocs-diff-footnotes--key (element)
  "Return semantic identity for footnote ELEMENT without addressing metadata."
  (format "footnote:%s:%s:%s" (or (plist-get element :label) "")
          (gdocs-convert--runs-to-plain-text (plist-get element :contents))
          (gdocs-diff--runs-formatting-key (plist-get element :contents))))

(defun gdocs-diff-footnotes--element (element)
  "Describe a footnote ELEMENT, or decline other kinds."
  (when (eq (plist-get element :type) 'footnote)
    (list :key (gdocs-diff-footnotes--key element) :body-length 0)))

(defun gdocs-diff-footnotes--reference-requests (run index)
  "Emit native reference requests for RUN at INDEX when a handler is active."
  (when gdocs-convert-footnote-reference-handler
    (funcall gdocs-convert-footnote-reference-handler run index)))

(defun gdocs-diff-footnotes--run (run)
  "Describe semantic footnote RUN, or decline other runs."
  (when-let* ((label (plist-get run :footnote-label)))
    (list :text (format "@footnote(%s)" label) :key (format "F(%s)" label)
          :normalize-whitespace t :requests #'gdocs-diff-footnotes--reference-requests)))

(defun gdocs-diff-footnotes--body-operations (old-ir new-ir operations)
  "Remove segment-only operations without losing cross-kind body changes.
OLD-IR and NEW-IR supply the elements addressed by OPERATIONS."
  (mapcan
   (lambda (op)
     (let ((old-note (and (plist-member op :old-index)
                          (eq (plist-get (nth (plist-get op :old-index) old-ir) :type) 'footnote)))
           (new-note (and (plist-member op :new-index)
                          (eq (plist-get (nth (plist-get op :new-index) new-ir) :type) 'footnote))))
       (cond
        ((and old-note new-note) nil)
        (old-note (when (eq (plist-get op :op) 'modify)
                    (list (list :op 'insert :new-index (plist-get op :new-index)))))
        (new-note (when (eq (plist-get op :op) 'modify)
                    (list (list :op 'delete :old-index (plist-get op :old-index)))))
        (t (list op))))) operations))

(defun gdocs-diff-footnotes--plan (old-ir new-ir operations _start)
  "Plan native segment updates for OLD-IR and NEW-IR before body OPERATIONS."
  (when (or gdocs-convert-footnote-reference-text-function
            (seq-some (lambda (element)
                        (and (eq (plist-get element :type) 'footnote)
                             (plist-get element :segment-id))) old-ir))
    (let (requests)
      (dolist (old old-ir)
        (when (and (eq (plist-get old :type) 'footnote) (plist-get old :segment-id))
          (when-let* ((new (seq-find (lambda (entry)
                                       (and (eq (plist-get entry :type) 'footnote)
                                            (equal (plist-get old :label) (plist-get entry :label)))) new-ir)))
            (unless (equal (gdocs-diff-footnotes--key old) (gdocs-diff-footnotes--key new))
              (setq requests (append requests (gdocs-convert-footnote-update-requests old new)))))))
      ;; Update live segments before body edits can remove their references.
      (list :operations (gdocs-diff-footnotes--body-operations old-ir new-ir operations)
            :before requests))))

(add-hook 'gdocs-diff-semantic-element-functions #'gdocs-diff-footnotes--element t)
(add-hook 'gdocs-diff-semantic-run-functions #'gdocs-diff-footnotes--run t)
(add-hook 'gdocs-diff-semantic-plan-functions #'gdocs-diff-footnotes--plan t)

(provide 'gdocs-diff-footnotes)
;;; gdocs-diff-footnotes.el ends here
