;;; gdocs-preamble.el --- Preserve local Org preambles -*- lexical-binding: t; -*-

;;; Commentary:
;; Raw local metadata is independent of document conversion and transport.
;; Parse only structural boundaries; never convert the document to locate them.

;;; Code:

(require 'org)
(require 'org-element)
(require 'seq)
(require 'subr-x)

(defun gdocs-preamble--body-start (element)
  "Return the first body position under Org ELEMENT, or nil."
  (pcase (org-element-type element)
    ((or 'org-data 'section)
     (seq-some #'gdocs-preamble--body-start (org-element-contents element)))
    ((or 'keyword 'property-drawer 'comment 'comment-block) nil)
    (_ (org-element-property :begin element))))

(defun gdocs-preamble-split (text)
  "Split Org TEXT into raw :preamble and :body strings.
Leading metadata, comments and whitespace precede the first structural body
node.  Affiliated attributes belong to that body node, not to the preamble."
  (with-temp-buffer
    (insert text)
    (let ((org-inhibit-startup t)) (delay-mode-hooks (org-mode)))
    (let ((start (or (gdocs-preamble--body-start (org-element-parse-buffer 'element))
                     (point-max))))
      (list :preamble (buffer-substring-no-properties (point-min) start)
            :body (buffer-substring-no-properties start (point-max))))))

(defun gdocs-preamble-preserve (local-org remote-org)
  "Preserve LOCAL-ORG's raw metadata preamble over REMOTE-ORG's rendered body.
Local keyword spelling, values and property metadata remain authoritative.
Without a nonblank local preamble, retain the complete remote rendering."
  (let ((preamble (plist-get (gdocs-preamble-split local-org) :preamble)))
    (if (string-blank-p preamble) remote-org
      (concat preamble (plist-get (gdocs-preamble-split remote-org) :body)))))

(provide 'gdocs-preamble)
;;; gdocs-preamble.el ends here
