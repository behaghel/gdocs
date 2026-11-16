;;; gdocs-diff-lists.el --- List paragraph diff policy -*- lexical-binding: t; -*-

;;; Commentary:
;; List identity, membership, nesting prefixes and native bullet lifecycle.
;; The generic diff kernel consumes a neutral paragraph descriptor and finisher.

;;; Code:

(require 'cl-lib)
(require 'gdocs-convert-lists)

(defun gdocs-diff--list-key (list-info)
  "Return a stable identity fragment for LIST-INFO, excluding transient counters."
  (if list-info
      (concat (format "%s:%d:%s" (plist-get list-info :type)
                      (plist-get list-info :level) (plist-get list-info :checked))
              (when (plist-get list-info :partial) ":partial"))
    ""))

(defun gdocs-diff--same-list-p (old-elem new-elem)
  "Return non-nil for OLD-ELEM and NEW-ELEM with the same list type and level."
  (let ((old (plist-get old-elem :list)) (new (plist-get new-elem :list)))
    (and old new (eq (plist-get old :type) (plist-get new :type))
         (= (or (plist-get old :level) 0) (or (plist-get new :level) 0)))))

(defun gdocs-diff--make-delete-bullets-request (start end)
  "Create a native bullet removal request for START..END, excluding its newline."
  `((deleteParagraphBullets . ((range . ((startIndex . ,start) (endIndex . ,(1- end))))))))

(defun gdocs-diff-lists-paragraph (element old)
  "Describe ELEMENT's structure, physical prefix and compatibility with OLD."
  (list :structure-key (gdocs-diff--list-key (plist-get element :list))
        :prefix (gdocs-convert--list-leading-prefix element)
        :compatible (or (and (null (plist-get old :list)) (null (plist-get element :list)))
                        (gdocs-diff--same-list-p old element))
        :requests #'gdocs-convert--make-list-requests))

(defun gdocs-diff-lists-finish (old new range result stage)
  "Finalize OLD to NEW paragraph RESULT at RANGE for modification STAGE."
  (pcase stage
    ('content
     ;; Keep the existing native list object and its alternated preset when
     ;; the paragraph's type/level is unchanged.
     (when (plist-get (gdocs-diff-lists-paragraph new old) :compatible)
       (plist-put result :insert-reqs
                  (cl-remove-if (lambda (request) (alist-get 'createParagraphBullets request))
                                (plist-get result :insert-reqs)))))
    ('modification
     ;; A full structural replacement has already removed the old paragraph.
     ;; Only a surviving paragraph can retain obsolete list membership.
     (when (and (eq (plist-get new :type) 'paragraph)
                (plist-get old :list) (not (plist-get new :list)))
       (let* ((text (gdocs-convert--runs-to-plain-text (plist-get new :contents)))
              (end (+ (car range) (gdocs-convert--string-to-utf16-length text) 1)))
         (plist-put result :style-reqs
                    (append (plist-get result :style-reqs)
                            (list (gdocs-diff--make-delete-bullets-request (car range) end))))))))
  result)

(provide 'gdocs-diff-lists)
;;; gdocs-diff-lists.el ends here
