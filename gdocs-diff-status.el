;;; gdocs-diff-status.el --- Atomic static status diff support -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pablo Stafforini

;;; Commentary:
;; Status-specific tokenization and provider marker lifecycle.  Generic token
;; splitting comes from gdocs-diff; shared UTF-16 lengths come from conversion.

;;; Code:

(require 'cl-lib)

(declare-function gdocs-diff--tokenize-text "gdocs-diff" (text))
(declare-function gdocs-convert--string-to-utf16-length "gdocs-convert" (string))

(defun gdocs-diff--status-marker-delete-requests (runs)
  "Delete provider marker IDs on status RUNS before changing their text."
  (mapcar (lambda (id) `((deleteNamedRange . ((namedRangeId . ,id)))))
          (delete-dups (cl-mapcan
                        (lambda (run) (copy-sequence (plist-get run :status-marker-ids))) runs))))

(defun gdocs-diff--tokenize-status-runs (runs)
  "Tokenize RUNS while keeping each semantic status as one atomic token."
  (let ((tokens nil) (plain nil) (offset 0))
    (cl-labels ((flush ()
                  (when plain
                    (let ((text (apply #'concat (nreverse plain))))
                      (dolist (token (gdocs-diff--tokenize-text text))
                        (plist-put token :offset (+ offset (plist-get token :offset)))
                        (push token tokens))
                      (setq offset (+ offset (gdocs-convert--string-to-utf16-length text))
                            plain nil)))))
      (dolist (run runs)
        (when (or (plist-get run :date-element) (plist-get run :footnote-label))
          (user-error "Status labels mixed with native dates or footnotes in one paragraph are unsupported"))
        (if (plist-get run :status)
            (progn
              (flush)
              (let* ((text (plist-get run :text))
                     (length (gdocs-convert--string-to-utf16-length text)))
                (push (list :text text :offset offset :length length
                            :status (plist-get run :status)
                            :status-marker-ids (plist-get run :status-marker-ids)
                            :key (list 'status (plist-get run :status) (substring-no-properties text))) tokens)
                (setq offset (+ offset length))))
          (push (plist-get run :text) plain)))
      (flush))
    (nreverse tokens)))

(provide 'gdocs-diff-status)
;;; gdocs-diff-status.el ends here
