;;; gdocs-diff-links.el --- Exact native link mutation policy -*- lexical-binding: t; -*-

;;; Commentary:
;; Clear actual native link spans before text moves.  Existing body emitters
;; restore authored links after mutation.  Unchanged semantic text may still
;; need a precise repair for an unintended whitespace-only hyperlink.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'gdocs-convert-links)
(defvar gdocs-diff-semantic-plan-functions)

(defun gdocs-diff-links--plan (old-ir new-ir operations _start)
  "Plan exact link clearing for OLD-IR to NEW-IR OPERATIONS before body edits."
  (let (requests seen)
    (dolist (operation operations)
      (when (memq (plist-get operation :op) '(keep modify))
        (let* ((old (nth (plist-get operation :old-index) old-ir))
               (new (nth (plist-get operation :new-index) new-ir))
               (modified (and (eq (plist-get operation :op) 'modify)
                              (eq (plist-get old :type) 'paragraph))))
          (dolist (span (plist-get old :native-link-spans))
            (when (and (or modified
                           (and (string-blank-p (plist-get span :text))
                                (not (gdocs-convert-links-source-whitespace-link-p new (plist-get span :url)))))
                       (not (member (cons (plist-get span :start) (plist-get span :end)) seen)))
              (push (cons (plist-get span :start) (plist-get span :end)) seen)
              (push (gdocs-convert-links-clear-span-request span) requests))))))
    (when requests (list :before (nreverse requests)))))

;; Clear old-coordinate links before other plans can mutate the document body.
(add-hook 'gdocs-diff-semantic-plan-functions #'gdocs-diff-links--plan)

(provide 'gdocs-diff-links)
;;; gdocs-diff-links.el ends here
