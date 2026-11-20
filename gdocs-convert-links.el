;;; gdocs-convert-links.el --- Native link addressing metadata -*- lexical-binding: t; -*-

;;; Commentary:
;; Retain exact native link spans independently of semantic run normalization.
;; Adjacent native style fragments with the same URL form one link span.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'gdocs-convert-text)

(declare-function gdocs-convert--extract-link-url "gdocs-link-resolve" (link-obj))

(defun gdocs-convert-links-run-policy (run)
  "Preserve whitespace belonging to linked RUN, including fragmented labels."
  (when (plist-get run :link) (list :preserve-whitespace t)))

(defun gdocs-convert-links--paragraph-spans (paragraph start)
  "Return exact native link spans in PARAGRAPH, falling back to START."
  (let ((cursor start) spans)
    (seq-doseq (part (alist-get 'elements paragraph))
      (let* ((run (alist-get 'textRun part)) (text (alist-get 'content run))
             (begin (or (alist-get 'startIndex part) cursor))
             (url (when (alist-get 'link (alist-get 'textStyle run))
                    (gdocs-convert--extract-link-url (alist-get 'link (alist-get 'textStyle run)))))
             (content (and (stringp text) (string-remove-suffix "\n" text))))
        (when (and url (integerp begin) content (not (string-empty-p content)))
          (let ((end (+ begin (gdocs-convert--string-to-utf16-length content))) (previous (car spans)))
            (if (and previous (equal (plist-get previous :url) url) (= (plist-get previous :end) begin))
                (progn (plist-put previous :end end)
                       (plist-put previous :text (concat (plist-get previous :text) content)))
              (push (list :start begin :end end :url url :text content) spans))))
        (setq cursor (or (alist-get 'endIndex part)
                         (and (integerp begin) (stringp text)
                              (+ begin (gdocs-convert--string-to-utf16-length text)))))))
    (nreverse spans)))

(defun gdocs-convert-links--native-spans (content)
  "Collect native paragraph link spans recursively from body CONTENT."
  (let (spans)
    (cl-labels ((visit (node)
                  (cond ((vectorp node) (seq-doseq (entry node) (visit entry)))
                        ((consp node)
                         (if (and (consp (car node)) (assq 'paragraph node))
                             (setq spans (nconc spans (gdocs-convert-links--paragraph-spans
                                                       (alist-get 'paragraph node) (alist-get 'startIndex node))))
                           (visit (car node)) (visit (cdr node)))))))
               (visit content))
    spans))

(defun gdocs-convert-links-annotate-native-ir (ir content)
  "Attach exact CONTENT link spans to their containing IR body elements.
These are native addressing metadata, not authored content identity."
  (let ((spans (gdocs-convert-links--native-spans content)))
    (dolist (element ir)
      (let ((start (plist-get element :doc-start)) (end (plist-get element :doc-end)))
        (when (and (not (plist-get element :segment-id)) (integerp start) (integerp end))
          (when-let* ((owned (seq-filter (lambda (span) (and (<= start (plist-get span :start))
                                                             (<= (plist-get span :end) end))) spans)))
            (plist-put element :native-link-spans owned)))))
    ir))

(defun gdocs-convert-links-clear-span-request (span)
  "Clear a native link at exact SPAN bounds without changing text."
  `((updateTextStyle . ((textStyle . ((underline . :json-false)))
                        (range . ((startIndex . ,(plist-get span :start)) (endIndex . ,(plist-get span :end))))
                        (fields . "link")))))

(defun gdocs-convert-links-source-whitespace-link-p (element url)
  "Return non-nil when source ELEMENT explicitly authors a whitespace link to URL."
  (cl-labels ((visit (node)
                (cond ((vectorp node) (seq-some #'visit node))
                      ((consp node)
                       (or (and (keywordp (car node)) (stringp (plist-get node :text))
                                (string-blank-p (plist-get node :text)) (equal (plist-get node :link) url))
                           (visit (car node)) (visit (cdr node)))))))
             (visit element)))

(provide 'gdocs-convert-links)
;;; gdocs-convert-links.el ends here
