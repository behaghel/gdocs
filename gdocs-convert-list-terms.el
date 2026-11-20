;;; gdocs-convert-list-terms.el --- Definition-term presentation provenance -*- lexical-binding: t; -*-

;;; Commentary:
;; Definition terms have intrinsic presentation, not implicit authored emphasis.
;; Inline provenance separates generated bold from source bold and keeps native
;; ranges independent of paragraph/list geometry.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'gdocs-convert-text)
(require 'gdocs-style-resolve)
(declare-function gdocs-convert--run-model-length "gdocs-convert" (run))

(defun gdocs-convert-list-term-source-runs (runs)
  "Annotate definition-term RUNS without adding authored bold."
  (mapcar (lambda (run)
            (when (or (plist-get run :date-element) (plist-get run :footnote-label) (plist-get run :status) (plist-get run :inline-source))
              (user-error "Definition terms currently support rich text, not dates, notes, statuses or inline source"))
            (let ((copy (copy-sequence run)))
              (plist-put copy :definition-term t)
              (plist-put copy :style-names (cons 'gdocs-definition-term (remq 'gdocs-definition-term (plist-get copy :style-names))))
              copy)) runs))

(defun gdocs-convert-list-term-run-policy (run)
  "Keep term RUN boundaries and whitespace distinct from their definition."
  (when (plist-get run :definition-term)
    (list :identity (list 'definition-term (plist-get run :inline-source)) :preserve-whitespace t)))

(defun gdocs-convert-list-term-run-requests (run start end segment-id)
  "Publish term RUN's authored-bold provenance at START..END in SEGMENT-ID."
  (when (plist-get run :definition-term)
    (when segment-id (user-error "Definition terms require a body list"))
    (let ((authored (not (null (plist-get run :bold))))
          (applied (or (plist-get run :bold)
                       (eq (alist-get 'bold (gdocs-convert--logical-style-text-fields 'gdocs-definition-term)) t))))
      (list `((createNamedRange
               . ((name . ,(format "gdocs-org-marker:definition-term:run-%d:v1:%d:%d" start (if authored 1 0) (if applied 1 0)))
                  (range . ,(gdocs-convert--text-range start end)))))))))

(defun gdocs-convert-list-term-native-parts (element markers parts)
  "Decorate decoded ELEMENT PARTS with inline term MARKERS."
  (let* ((cursor (or (alist-get 'startIndex element) 0))
         (terms (seq-filter (lambda (marker) (eq (plist-get marker :type) 'definition-term)) markers)) result)
    (dolist (pair parts (nreverse result))
      (let* ((run (cdr pair)) (length (gdocs-convert--run-model-length run))
             (end (+ cursor length))
             (active (seq-filter (lambda (marker) (and (< (plist-get marker :start) end) (> (plist-get marker :end) cursor))) terms))
             (cuts (sort (delete-dups (append (list cursor end)
                                              (cl-mapcan (lambda (marker) (list (max cursor (plist-get marker :start))
                                                                                (min end (plist-get marker :end)))) active))) #'<)))
        (if (null active)
            (push pair result)
          (while (cdr cuts)
            (let* ((a (pop cuts)) (b (car cuts))
                   (covering (seq-filter (lambda (marker) (and (<= (plist-get marker :start) a) (>= (plist-get marker :end) b))) active))
                   (marker (car covering)) (copy (copy-sequence run)))
              (when (> (length covering) 1) (user-error "Overlapping definition-term provenance"))
              (unless (and (= a cursor) (= b end))
                (plist-put copy :text (substring (plist-get run :text)
                                                 (gdocs-convert--utf16-string-index (plist-get run :text) (- a cursor))
                                                 (gdocs-convert--utf16-string-index (plist-get run :text) (- b cursor)))))
              (when marker
                (when (or (plist-get run :date-element) (plist-get run :footnote-label) (plist-get run :status) (plist-get run :inline-source))
                  (user-error "Unsupported semantic object inside a definition term"))
                (let ((data (plist-get marker :data)))
                  (unless (equal data "legacy")
                    (unless (and (stringp data) (string-match "\\`v1:\\([01]\\):\\([01]\\)\\'" data))
                      (user-error "Invalid definition-term provenance"))
                    (let ((authored (equal (match-string 1 data) "1")) (applied (equal (match-string 2 data) "1")))
                      (when (and authored (not applied)) (user-error "Invalid definition-term bold provenance"))
                      (unless (eq (not (null (plist-get copy :bold))) applied)
                        (user-error "Native definition-term bold differs from its presentation; review the term before pulling"))
                      (plist-put copy :bold authored))))
                (plist-put copy :definition-term t)
                (plist-put copy :style-names (cons 'gdocs-definition-term (remq 'gdocs-definition-term (plist-get copy :style-names))))
                (when-let* ((id (plist-get marker :range-id))) (plist-put copy :definition-term-range-ids (list id))))
              (push (cons (car pair) copy) result))))
        (setq cursor end)))))

(defun gdocs-convert-list-term-infer-legacy (runs)
  "Recognize an unmarked legacy list's plain term delimiter in RUNS.
Without provenance, native bold remains authored; it is never silently stripped."
  (if (seq-some (lambda (run) (plist-get run :definition-term)) runs) runs
    (let ((text (gdocs-convert--runs-to-plain-text runs)) (search 0) end)
      (while (and (not end) (string-match " :: " text search))
        (let* ((position (match-beginning 0))
               (candidate (gdocs-convert--string-to-utf16-length (substring text 0 position)))
               (cursor 0) invalid)
          (setq search (1+ position))
          (dolist (run runs)
            (let ((next (+ cursor (gdocs-convert--run-model-length run))))
              (when (and (< cursor (+ candidate 4)) (> next candidate)
                         (or (plist-get run :code) (plist-get run :link) (plist-get run :status)
                             (plist-get run :date-element) (plist-get run :footnote-label)))
                (setq invalid t))
              (setq cursor next)))
          (unless invalid (setq end candidate))))
      (if (and end (> end 0))
          (mapcar #'cdr (gdocs-convert-list-term-native-parts
                         '((startIndex . 0)) (list (list :type 'definition-term :start 0 :end end :data "legacy"))
                         (mapcar (lambda (run) (cons nil run)) runs))) runs))))

(defun gdocs-convert-list-term-paragraph-markers (markers)
  "Exclude inline-owned term MARKERS from paragraph identity."
  (seq-remove (lambda (marker) (eq (plist-get marker :type) 'definition-term)) markers))

(provide 'gdocs-convert-list-terms)
;;; gdocs-convert-list-terms.el ends here
