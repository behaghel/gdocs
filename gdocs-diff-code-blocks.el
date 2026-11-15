;;; gdocs-diff-code-blocks.el --- Code/example diff semantics -*- lexical-binding: t; -*-

;;; Commentary:
;; Source language, style revision, literal lines and newline addressing belong
;; to the code/example feature, not the generic LCS and index algorithms.

;;; Code:

(require 'gdocs-convert-text)
(defvar gdocs-diff-semantic-element-functions)
(defvar gdocs-diff-semantic-run-functions)
(defvar gdocs-diff-semantic-plan-functions)
(declare-function gdocs-convert-code-run-marker-request "gdocs-convert-code-blocks" (run start end &optional segment-id))

(defun gdocs-diff--source-block-key (element)
  "Compute a content key for a source-block ELEMENT."
  (format "source-block:%s:%s:%s" (or (plist-get element :language) "")
          (or (plist-get element :style-revision) "")
          (mapconcat #'identity (plist-get element :lines) "\n")))

(defun gdocs-diff--example-block-key (element)
  "Compute a content key for example-block ELEMENT."
  (format "example-block:%s" (mapconcat #'identity (plist-get element :lines) "\n")))

(defun gdocs-diff-code-blocks--body-length (element)
  "Return literal UTF-16 line lengths plus terminal newlines for ELEMENT."
  (apply #'+ (mapcar (lambda (line) (1+ (gdocs-convert--string-to-utf16-length line)))
                     (or (plist-get element :lines) '("")))))

(defun gdocs-diff--source-block-utf16-length (element)
  "Compute UTF-16 length of source-block ELEMENT."
  (gdocs-diff-code-blocks--body-length element))

(defun gdocs-diff--example-block-utf16-length (element)
  "Compute UTF-16 length of example-block ELEMENT."
  (gdocs-diff-code-blocks--body-length element))

(defun gdocs-diff-code-blocks--element (element)
  "Describe code/example ELEMENT, or decline other kinds."
  (pcase (plist-get element :type)
    ('source-block (list :key (gdocs-diff--source-block-key element)
                         :body-length (gdocs-diff--source-block-utf16-length element)))
    ('example-block (list :key (gdocs-diff--example-block-key element)
                          :body-length (gdocs-diff--example-block-utf16-length element)))))

(add-hook 'gdocs-diff-semantic-element-functions #'gdocs-diff-code-blocks--element t)

(defun gdocs-diff-code-blocks--run (run)
  "Describe inline source RUN's semantic language/header identity."
  (when-let* ((source (plist-get run :inline-source)))
    (list :key (format "inline-source:%S" source) :normalize-whitespace nil
          :requests (lambda (value index)
                      (list (gdocs-convert-code-run-marker-request
                             value index (+ index (gdocs-convert--string-to-utf16-length (plist-get value :text)))))))))

(declare-function gdocs-diff--runs-formatting-key "gdocs-diff" (runs))

(defun gdocs-diff-code-blocks--plan (old new operations _start)
  "Remove stale inline source annotations before OPERATIONS mutate OLD content."
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
          (dolist (run (plist-get element :contents))
            (setq ids (append ids (plist-get run :inline-source-range-ids)))))))
    (when ids
      (list :before (mapcar (lambda (id) `((deleteNamedRange . ((namedRangeId . ,id))))) (delete-dups ids))))))

(add-hook 'gdocs-diff-semantic-run-functions #'gdocs-diff-code-blocks--run t)
(add-hook 'gdocs-diff-semantic-plan-functions #'gdocs-diff-code-blocks--plan t)

(provide 'gdocs-diff-code-blocks)
;;; gdocs-diff-code-blocks.el ends here
