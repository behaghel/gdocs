;;; gdocs-diff-tasks.el --- Managed task physical projection -*- lexical-binding: t; -*-

;;; Commentary:
;; Diff physical glyph text and indices while keeping semantic Org task IR
;; independent of presentation.  Recreate changed glyph range annotations.

;;; Code:

(require 'gdocs-convert-tasks)
(defvar gdocs-diff-semantic-element-functions)
(defvar gdocs-diff-semantic-run-functions)
(defvar gdocs-diff-semantic-plan-functions)
(declare-function gdocs-diff--runs-formatting-key "gdocs-diff" (runs))

(defun gdocs-diff-tasks-project-ir (ir)
  "Project semantic tasks in IR to their physical document representation."
  (mapcar #'gdocs-convert-task-project-paragraph ir))

(defun gdocs-diff-tasks--element (element)
  "Describe semantic task ELEMENT's physical body length."
  (when (gdocs-convert-task-paragraph-p element)
    (list :body-length (+ 3 (gdocs-convert--string-to-utf16-length
                             (gdocs-convert--runs-to-plain-text (plist-get element :contents)))))))

(defun gdocs-diff-tasks--run (run)
  "Describe managed task prefix RUN and its annotation requests."
  (when-let* ((info (plist-get run :managed-task-prefix)))
    (list :key (format "task-prefix:%d" (plist-get info :level))
          :normalize-whitespace nil :requests #'gdocs-convert-task-run-requests)))

(defun gdocs-diff-tasks--plan (old new operations _start)
  "Delete stale managed glyph ranges before content OPERATIONS on OLD and NEW."
  (let (ids)
    (dolist (operation operations)
      (let* ((kind (plist-get operation :op))
             (element (when (memq kind '(modify delete)) (nth (plist-get operation :old-index) old)))
             (updated (when (eq kind 'modify) (nth (plist-get operation :new-index) new))))
        (when (and element
                   (or (eq kind 'delete)
                       (not (equal (gdocs-convert--runs-to-plain-text (plist-get element :contents))
                                   (gdocs-convert--runs-to-plain-text (plist-get updated :contents))))
                       (not (equal (gdocs-diff--runs-formatting-key (plist-get element :contents))
                                   (gdocs-diff--runs-formatting-key (plist-get updated :contents))))))
          (setq ids (append ids (plist-get element :managed-task-range-ids))))))
    (when ids
      (list :before (mapcar (lambda (id) `((deleteNamedRange . ((namedRangeId . ,id))))) (delete-dups (delq nil ids)))))))

(add-hook 'gdocs-diff-semantic-element-functions #'gdocs-diff-tasks--element t)
(add-hook 'gdocs-diff-semantic-run-functions #'gdocs-diff-tasks--run t)
(add-hook 'gdocs-diff-semantic-plan-functions #'gdocs-diff-tasks--plan t)

(provide 'gdocs-diff-tasks)
;;; gdocs-diff-tasks.el ends here
