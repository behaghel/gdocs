;;; gdocs-diff-status.el --- Atomic static status diff support -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pablo Stafforini

;;; Commentary:
;; Status-specific tokenization and provider marker lifecycle.  Generic token
;; splitting comes from gdocs-diff; shared UTF-16 lengths come from conversion.

;;; Code:

(require 'cl-lib)
(require 'gdocs-convert-status)

(defvar gdocs-diff-semantic-run-functions)

(declare-function gdocs-diff--tokenize-text "gdocs-diff" (text))
(declare-function gdocs-convert--string-to-utf16-length "gdocs-convert" (string))
(declare-function gdocs-convert-status--uniform-label "gdocs-convert-status" (status label))

(defun gdocs-diff--status-marker-delete-requests (runs)
  "Delete provider marker IDs on status RUNS before changing their text."
  (mapcar (lambda (id) `((deleteNamedRange . ((namedRangeId . ,id)))))
          (delete-dups (cl-mapcan
                        (lambda (run) (copy-sequence (plist-get run :status-marker-ids))) runs))))

(defun gdocs-diff-status--run (run)
  "Describe static status RUN identity, physical width and mutation policy."
  (when (plist-get run :status)
    (list :text (format "@status%S" (list (plist-get run :status)
                                          (substring-no-properties (plist-get run :text))))
          :key (format "T%S" (plist-get run :status))
          :key-length (gdocs-convert--string-to-utf16-length (plist-get run :text))
          :content-change t :tokenizer #'gdocs-diff--tokenize-status-runs
          :delete-requests #'gdocs-diff--status-marker-delete-requests)))

(defun gdocs-diff-status--retain-token-p (old new identical)
  "Retain matched OLD/NEW metadata unless IDENTICAL text hides a moved boundary."
  (not (and identical (/= (plist-get old :offset) (plist-get new :offset)))))

(defun gdocs-diff-status--token-marker (status length index)
  "Emit STATUS metadata of physical LENGTH at INDEX after insertion planning."
  (gdocs-convert--status-marker-request status index (+ index length)))

(defun gdocs-diff--tokenize-status-runs (runs &optional native)
  "Tokenize RUNS, keeping each status atomic and its emitted spelling separate.
With NATIVE, use actual fetched text rather than current presentation."
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
                     (status (plist-get run :status))
                     (visible (if native (or (plist-get run :status-native-text) text)
                                (gdocs-convert-status--uniform-label status text)))
                     (length (gdocs-convert--string-to-utf16-length visible))
                     (provenance (plist-put (copy-sequence status) :source-label text)))
                (push (list :text visible :offset offset :length length
                            :status provenance
                            :status-marker-ids (plist-get run :status-marker-ids)
                            :key (list 'status (plist-get run :status) (substring-no-properties text))
                            :retain-p #'gdocs-diff-status--retain-token-p
                            :insert-requests (apply-partially #'gdocs-diff-status--token-marker provenance length)) tokens)
                (setq offset (+ offset length))))
          (push (plist-get run :text) plain)))
      (flush))
    (nreverse tokens)))

(add-hook 'gdocs-diff-semantic-run-functions #'gdocs-diff-status--run t)

(provide 'gdocs-diff-status)
;;; gdocs-diff-status.el ends here
