;;; gdocs-diff-list-continuations.el --- Continuation annotation diff -*- lexical-binding: t; -*-

;;; Commentary:
;; Continuations are ordinary physical paragraphs with a zero-width annotation
;; emitter.  Identity, nesting, indentation and stale range policy stay here.

;;; Code:

(require 'gdocs-convert-lists)
(defvar gdocs-diff-semantic-run-functions)
(defvar gdocs-diff-semantic-plan-functions)
(declare-function gdocs-diff--runs-formatting-key "gdocs-diff" (runs))
(declare-function gdocs-diff--compute-element-indices "gdocs-diff" (ir &optional start-index))

(defun gdocs-diff-list-continuations--run (run)
  "Describe continuation annotation RUN's identity and request emitter."
  (when-let* ((info (plist-get run :list-continuation-prefix)))
    (list :key (format "list-continuation:%s:%d" (plist-get info :type) (plist-get info :level))
          :normalize-whitespace nil :requests #'gdocs-convert-list-continuation-run-requests)))

(defun gdocs-diff-list-continuations--plan (old new operations start)
  "Remove stale continuation annotations before OPERATIONS mutate OLD and NEW."
  (let (indices ids clears)
    (dolist (operation operations)
      (let* ((kind (plist-get operation :op))
             (element (when (memq kind '(modify delete)) (nth (plist-get operation :old-index) old)))
             (updated (when (eq kind 'modify) (nth (plist-get operation :new-index) new))))
        (when (and (gdocs-convert-list-continuation-p element)
                   (not (gdocs-convert-list-continuation-p updated)))
          (unless indices (setq indices (gdocs-diff--compute-element-indices old start)))
          (let ((index (car (cdr (assq (plist-get operation :old-index) indices)))))
            (push `((updateParagraphStyle
                     . ((paragraphStyle . ((indentStart . ((magnitude . 0) (unit . "PT")))
                                           (indentFirstLine . ((magnitude . 0) (unit . "PT")))))
                        (range . ((startIndex . ,index) (endIndex . ,(1+ index))))
                        (fields . "indentStart,indentFirstLine")))) clears)))
        (when (and element
                   (or (eq kind 'delete)
                       (not (equal (gdocs-convert--runs-to-plain-text (plist-get element :contents))
                                   (gdocs-convert--runs-to-plain-text (plist-get updated :contents))))
                       (not (equal (gdocs-diff--runs-formatting-key (plist-get element :contents))
                                   (gdocs-diff--runs-formatting-key (plist-get updated :contents))))))
          (setq ids (append ids (plist-get element :list-continuation-range-ids))))))
    (when (or ids clears)
      (list :before (append (mapcar (lambda (id) `((deleteNamedRange . ((namedRangeId . ,id)))))
                                    (delete-dups (delq nil ids)))
                            (nreverse clears))))))

(add-hook 'gdocs-diff-semantic-run-functions #'gdocs-diff-list-continuations--run t)
(add-hook 'gdocs-diff-semantic-plan-functions #'gdocs-diff-list-continuations--plan t)

(provide 'gdocs-diff-list-continuations)
;;; gdocs-diff-list-continuations.el ends here
