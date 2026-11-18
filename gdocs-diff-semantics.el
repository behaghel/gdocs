;;; gdocs-diff-semantics.el --- Semantic diff boundary and composition -*- lexical-binding: t; -*-

;;; Commentary:
;; Three ordinary Emacs hooks supply element descriptors, inline descriptors and
;; request plans.  The kernel consumes this neutral data; feature modules own
;; recognition and policy.  Features are loaded explicitly, not discovered.

;;; Code:

(require 'cl-lib)

(defvar gdocs-diff-semantic-element-functions nil
  "Functions accepting an IR element and returning a descriptor or nil.
The first matching descriptor supplies :key and :body-length.")

(defvar gdocs-diff-semantic-run-functions nil
  "Functions accepting an inline run and returning a descriptor or nil.
The first match may supply :text, :key, :normalize-whitespace and :requests.
A :requests function accepts the run and its UTF-16 body index.")

(defvar gdocs-diff-semantic-plan-functions nil
  "Functions accepting old IR, new IR, operations and a start index.
Each may return :operations, :before and :after requests.
Plans compose in hook order; explicit nil operations means no body operations,
not an unmatched plan.")

(declare-function gdocs-convert--run-model-length "gdocs-convert" (run))

(defun gdocs-diff-semantic-element (element)
  "Return the first semantic descriptor for ELEMENT, or nil."
  (run-hook-with-args-until-success 'gdocs-diff-semantic-element-functions element))

(defun gdocs-diff-semantic-run (run)
  "Return the first semantic descriptor for RUN, or nil."
  (run-hook-with-args-until-success 'gdocs-diff-semantic-run-functions run))

(defun gdocs-diff-semantic-run-requests (runs start)
  "Return semantic requests for RUNS at UTF-16 body index START."
  (let ((index start) requests)
    (dolist (run runs)
      (when-let* ((emit (plist-get (gdocs-diff-semantic-run run) :requests)))
        (setq requests (append requests (funcall emit run index))))
      (setq index (+ index (gdocs-convert--run-model-length run))))
    requests))

(defun gdocs-diff-semantic-plan (old-ir new-ir operations start)
  "Compose semantic plans for OLD-IR, NEW-IR, OPERATIONS and START."
  (let (before after)
    (dolist (function gdocs-diff-semantic-plan-functions)
      (when-let* ((plan (funcall function old-ir new-ir operations start)))
        (when (plist-member plan :operations)
          (setq operations (plist-get plan :operations)))
        (setq before (append before (plist-get plan :before))
              after (append after (plist-get plan :after)))))
    (list :operations operations :before before :after after)))

;; Explicit composition belongs here, not in the generic LCS/indexing engine.
(require 'gdocs-diff-footnotes)
(require 'gdocs-diff-tables)
(require 'gdocs-diff-images)
(require 'gdocs-diff-code-blocks)
(require 'gdocs-diff-quotes)
(require 'gdocs-diff-verse)
(require 'gdocs-diff-centers)
(require 'gdocs-diff-callouts)
(require 'gdocs-diff-tasks)
(require 'gdocs-diff-list-continuations)
(require 'gdocs-diff-lists)

(defun gdocs-diff-semantic-paragraph (element &optional old)
  "Describe ELEMENT's opaque paragraph structure and compatibility with OLD."
  (gdocs-diff-lists-paragraph element old))

(defun gdocs-diff-semantic-paragraph-requests (element start end)
  "Emit feature-owned paragraph requests for ELEMENT at START..END."
  (when-let* ((emit (plist-get (gdocs-diff-semantic-paragraph element) :requests)))
    (funcall emit element start end)))

(defun gdocs-diff-semantic-finish-paragraph (old new range result stage)
  "Compose feature-owned OLD to NEW paragraph RESULT finalization at RANGE.
STAGE identifies content planning or the complete modification envelope."
  (gdocs-diff-lists-finish old new range result stage))

(defun gdocs-diff-semantic-project-ir (ir)
  "Compose feature-owned physical projections before generic diffing of IR."
  (gdocs-convert-list-project-ir (gdocs-diff-tasks-project-ir ir)))

(provide 'gdocs-diff-semantics)
;;; gdocs-diff-semantics.el ends here
