;;; gdocs-convert-callouts.el --- Callout conversion and request policy -*- lexical-binding: t; -*-

;;; Commentary:
;; Own callout source/native semantics, labels, metadata, rendering, geometry
;; and callout-specific post-bullet repair.  Generic traversal and list services
;; supply ordered paragraphs and requests, never callout recognition policy.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org-element)
(require 'ox)
(require 'json)
(require 'url-util)
(require 'gdocs-convert-paragraphs)
(require 'gdocs-convert-inline-codec)
(require 'gdocs-convert-tasks)

(declare-function gdocs-convert--next-id "gdocs-convert" ())
(declare-function gdocs-convert--paragraph-child-to-ir "gdocs-convert" (paragraph))
(declare-function gdocs-convert--plain-list-positioned "gdocs-convert" (plain-list depth))
(declare-function gdocs-convert--assign-list-counters "gdocs-convert" (ir))
(declare-function gdocs-convert--ir-paragraph-to-org "gdocs-convert" (element))
(declare-function gdocs-convert--paragraph-to-requests "gdocs-convert" (element index))
(declare-function gdocs-convert--fixup-list-nesting "gdocs-convert" (requests element-ranges))
(declare-function gdocs-convert--list-prefix-length-for-element "gdocs-convert" (element))
(declare-function gdocs-convert--list-prefixes-before "gdocs-convert" (range element-ranges))
(declare-function gdocs-convert--list-marker "gdocs-convert-lists" (type list-info))

(defcustom gdocs-callout-types '("info" "note" "warning" "tip" "important")
  "Supported semantic Org callout types."
  :type '(repeat string) :group 'gdocs)

(defcustom gdocs-callout-labels
  '(("info" . "ℹ️ Info") ("note" . "📝 Note") ("warning" . "⚠️ Warning")
    ("tip" . "💡 Tip") ("important" . "❗ Important"))
  "Visible labels used for semantic callout types."
  :type '(alist :key-type string :value-type string) :group 'gdocs)

(defun gdocs-convert-callout-element-p (element)
  "Return non-nil for semantic callout ELEMENT."
  (eq (plist-get element :type) 'callout-block))

(defun gdocs-convert-callout-source-block (block)
  "Return callout IR elements for Org BLOCK, or nil when unhandled."
  (when (gdocs-convert--callout-block-p block) (list (gdocs-convert--callout-block-to-ir block))))

(defun gdocs-convert--callout-block-p (special-block)
  "Return non-nil if SPECIAL-BLOCK is the canonical callout block."
  (string= (upcase (or (org-element-property :type special-block) "")) "CALLOUT"))

(defun gdocs-convert--unquote-org-attribute (value)
  "Return VALUE without one layer of Org attribute string quoting."
  (cond ((not (stringp value)) value)
        ((string-match-p "\\`\".*\"\\'" value)
         (condition-case nil (car (read-from-string value))
           (error (string-trim value "\"" "\"")))) (t value)))

(defun gdocs-convert--callout-attributes (special-block)
  "Return callout attributes from SPECIAL-BLOCK."
  (let ((attr (org-export-read-attribute :attr_callout special-block)))
    (list :type (gdocs-convert--unquote-org-attribute (or (plist-get attr :type) "info"))
          :title (gdocs-convert--unquote-org-attribute (plist-get attr :title)))))

(defun gdocs-convert--validate-callout-type (type)
  "Signal an error unless TYPE is supported."
  (unless (member type gdocs-callout-types)
    (user-error "Unsupported Org callout type `%s'; supported types: %s" type (string-join gdocs-callout-types ", "))))

(defun gdocs-convert--reject-nested-block-list-elements (elements block-name)
  "Reject nested list ELEMENTS that cannot preserve full BLOCK-NAME shading."
  (when-let* ((nested (seq-find (lambda (element) (let ((info (plist-get element :list)))
                                                    (and info (> (or (plist-get info :level) 0) 0)))) elements)))
    (user-error "Unsupported Org %s nested list item `%s'; Google Docs paragraph shading cannot preserve nested list indentation and full block background in v1"
                block-name (gdocs-convert--runs-to-plain-text (plist-get nested :contents)))))

(defun gdocs-convert--block-body-elements (block allowed-types block-name)
  "Decode direct BLOCK children in ALLOWED-TYPES, diagnosing BLOCK-NAME loss."
  (let (entries)
    (dolist (child (org-element-contents block))
      (unless (stringp child)
        (let ((type (org-element-type child)))
          (unless (memq type allowed-types)
            (user-error "Unsupported Org %s child `%s'; v1 supports paragraphs and lists only" block-name type))
          (pcase type
            ('paragraph (when-let* ((ir (gdocs-convert--paragraph-child-to-ir child)))
                          (push (list :ir ir :begin (org-element-property :begin child)) entries)))
            ('plain-list (setq entries (nconc (gdocs-convert--plain-list-positioned child 0) entries)))))))
    (let ((elements (mapcar (lambda (entry) (plist-get entry :ir))
                            (sort entries (lambda (a b) (< (plist-get a :begin) (plist-get b :begin)))))))
      (gdocs-convert--reject-nested-block-list-elements elements block-name) elements)))

(defun gdocs-convert--callout-body-elements (special-block)
  "Return ordered body IR for SPECIAL-BLOCK."
  (gdocs-convert--block-body-elements special-block '(paragraph plain-list) "callout"))

(defun gdocs-convert--validate-callout-body (special-block)
  "Validate SPECIAL-BLOCK body before publication."
  (gdocs-convert--callout-body-elements special-block) nil)

(defun gdocs-convert--callout-block-to-ir (special-block)
  "Convert canonical Org SPECIAL-BLOCK to callout IR."
  (let* ((attrs (gdocs-convert--callout-attributes special-block))
         (type (or (plist-get attrs :type) "info")) (title (plist-get attrs :title))
         (body (gdocs-convert--callout-body-elements special-block)))
    (gdocs-convert--validate-callout-type type)
    (list :type 'callout-block :callout-type type :title title :body-elements body
          :paragraphs (mapcar (lambda (element) (plist-get element :contents)) body) :id (gdocs-convert--next-id))))

(defun gdocs-convert--callout-body-elements-from-paragraphs (element)
  "Return body elements for ELEMENT, accepting older paragraph-only IR."
  (or (plist-get element :body-elements)
      (mapcar (lambda (runs) (list :type 'paragraph :style 'normal :contents runs)) (plist-get element :paragraphs))))

(defun gdocs-convert--join-callout-body-org (elements)
  "Join callout body ELEMENTS while retaining list grouping."
  (gdocs-convert--assign-list-counters elements)
  (let (parts previous-list-p)
    (dolist (element elements)
      (let ((list-p (plist-get element :list)) (text (gdocs-convert--ir-paragraph-to-org element)))
        (when parts (push (if (and previous-list-p list-p) "\n" "\n\n") parts))
        (push text parts) (setq previous-list-p list-p)))
    (apply #'concat (nreverse parts))))

(defun gdocs-convert--ir-callout-block-to-org (element)
  "Render semantic callout ELEMENT as Org."
  (let* ((type (or (plist-get element :callout-type) "info")) (title (plist-get element :title))
         (attrs (concat "#+ATTR_CALLOUT: :type " type
                        (if (and title (not (string-empty-p title))) (format " :title %S" title) ""))))
    (format "%s\n#+BEGIN_CALLOUT\n%s\n#+END_CALLOUT" attrs
            (gdocs-convert--join-callout-body-org (gdocs-convert--callout-body-elements-from-paragraphs element)))))

(defun gdocs-convert--callout-marker-p (marker)
  "Return non-nil for callout grouping MARKER."
  (memq (plist-get marker :type) '(callout-label callout-body)))

(defun gdocs-convert--callout-marker (element)
  "Return the callout grouping marker on ELEMENT."
  (seq-find #'gdocs-convert--callout-marker-p (plist-get element :gdocs-marker)))

(defun gdocs-convert--callout-marker-data (marker)
  "Decode semantic callout MARKER data, restoring UTF-8 titles."
  (let* ((parts (split-string (or (plist-get marker :data) "info:") ":"))
         (type (or (car parts) "info"))
         (title (when (cadr parts) (decode-coding-string (url-unhex-string (cadr parts)) 'utf-8))))
    (gdocs-convert--validate-callout-type type)
    (list :type type :title (and title (not (string-empty-p title)) title))))

(defun gdocs-convert-callouts--native-title (element data)
  "Decode ELEMENT's visible title using the applied label prefix, not settings."
  (let* ((snapshot (seq-find (lambda (marker) (eq (plist-get marker :type) 'callout-label-base))
                             (plist-get element :gdocs-marker)))
         (visible (gdocs-convert--runs-to-plain-text (plist-get element :contents)))
         (original (plist-get data :title)))
    (if snapshot
        (let* ((base (decode-coding-string (url-unhex-string (or (plist-get snapshot :data) "")) 'utf-8))
               (prefix (concat base " — ")))
          (cond ((equal visible base) nil)
                ((string-prefix-p prefix visible)
                 (let ((title (substring visible (length prefix)))) (unless (string-empty-p title) title)))
                (t (user-error "Callout label prefix was edited; restore its recorded label or update the callout from authoritative Org"))))
      ;; Legacy markers carry a title but no presentation snapshot.  Retain
      ;; only provably unchanged titles; do not guess through an edited label.
      (cond ((and original (string-suffix-p (concat " — " original) visible)) original)
            ((and (null original) (not (string-match-p (regexp-quote " — ") visible))) nil)
            (t (user-error "Callout label provenance is missing; push authoritative Org before pulling title edits"))))))

(defun gdocs-convert--callout-list-marker (element)
  "Return callout list metadata on ELEMENT."
  (seq-find (lambda (marker) (eq (plist-get marker :type) 'callout-list)) (plist-get element :gdocs-marker)))

(defun gdocs-convert--callout-list-marker-p (element)
  "Return non-nil when ELEMENT is a callout list paragraph."
  (and (gdocs-convert--callout-list-marker element) t))

(defun gdocs-convert--callout-list-marker-data (marker)
  "Decode callout-list MARKER, retaining checkbox state and list shape."
  (let ((raw (or (plist-get marker :data) "bullet:0")))
    (if (string-prefix-p "v2:" raw)
        (let* ((data (condition-case nil
                         (json-parse-string (decode-coding-string (url-unhex-string (substring raw 3)) 'utf-8)
                                            :object-type 'alist :array-type 'list)
                       (error (user-error "Malformed callout list metadata"))))
               (valid-object (and (proper-list-p data) (seq-every-p #'consp data)))
               (type (and valid-object (alist-get 'type data)))
               (level (and valid-object (alist-get 'level data))))
          (unless (and (member type '("bullet" "number" "check")) (integerp level) (>= level 0))
            (user-error "Invalid callout list type or level"))
          (dolist (key '(checked partial))
            (when (and (assq key data) (not (memq (alist-get key data) '(0 1))))
              (user-error "Invalid callout task-state metadata")))
          (when (and (assq 'counter data)
                     (not (and (integerp (alist-get 'counter data)) (>= (alist-get 'counter data) 0))))
            (user-error "Invalid callout counter metadata"))
          (append (list :type (intern type) :level level)
                  (when (assq 'checked data) (list :checked (= (alist-get 'checked data) 1)))
                  (when (assq 'partial data) (list :partial (= (alist-get 'partial data) 1)))
                  (when (assq 'counter data) (list :counter (alist-get 'counter data)))))
      (unless (string-match-p "\\`\\(?:bullet\\|number\\|check\\):[0-9]+\\'" raw)
        (user-error "Invalid legacy callout list metadata"))
      (let* ((parts (split-string raw ":")) (type (intern (car parts)))
             (level (string-to-number (cadr parts))))
        (unless (and (memq type '(bullet number check)) (>= level 0)) (user-error "Invalid legacy callout list metadata"))
        (list :type type :level level)))))

(defun gdocs-convert--strip-callout-list-prefix (element list-info)
  "Remove a legacy visible LIST-INFO prefix from ELEMENT across native runs."
  (let* ((prefix (gdocs-convert--list-marker (plist-get list-info :type) list-info))
         (copy (copy-sequence element)) (runs (mapcar #'copy-sequence (plist-get element :contents))))
    (when (string-prefix-p prefix (gdocs-convert--runs-to-plain-text runs))
      (let ((left (length prefix)))
        (while (and runs (> left 0))
          (let* ((text (plist-get (car runs) :text)) (take (min left (length text))))
            (setq left (- left take))
            (if (= take (length text)) (setq runs (cdr runs))
              (plist-put (car runs) :text (substring text take)))))
        (plist-put copy :contents runs)))
    copy))

(defun gdocs-convert--restore-callout-list-element (element)
  "Restore callout list semantics on ELEMENT without stripping authored text."
  (if-let* ((marker (gdocs-convert--callout-list-marker element)))
      (let* ((info (gdocs-convert--callout-list-marker-data marker))
             (modern (string-prefix-p "v2:" (or (plist-get marker :data) "")))
             (copy (if (or modern (plist-get element :list)) (copy-sequence element)
                     (gdocs-convert--strip-callout-list-prefix element info))))
        (when (eq (plist-get info :type) 'check)
          (unless (gdocs-convert-task-paragraph-p element)
            (user-error "Callout task lacks an editable managed glyph; republish authoritative Org before pulling")))
        (plist-put copy :list
                   (cond ((gdocs-convert-task-paragraph-p element) (copy-sequence (plist-get element :list)))
                         (modern info) (t (append info (plist-get element :list))))) copy)
    element))

(defun gdocs-convert--attach-callout-block-markers (ir)
  "Group native callout paragraphs without guessing away native structure."
  (let (result (remaining ir))
    (while remaining
      (let* ((element (car remaining)) (marker (gdocs-convert--callout-marker element)))
        (if marker
            (let* ((data (gdocs-convert--callout-marker-data marker))
                   (type (plist-get data :type)) (title (plist-get data :title))
                   (first t) body (start (plist-get element :doc-start)) end)
              (when (eq (plist-get marker :type) 'callout-label)
                (setq title (gdocs-convert-callouts--native-title element data)))
              (while (and remaining
                          (let ((candidate (gdocs-convert--callout-marker (car remaining))))
                            (and candidate (or first (not (eq (plist-get candidate :type) 'callout-label)))
                                 (equal data (gdocs-convert--callout-marker-data candidate)))))
                (when (and (eq (plist-get (gdocs-convert--callout-marker (car remaining)) :type) 'callout-body)
                           (not (gdocs-convert--empty-paragraph-p (car remaining))))
                  (push (gdocs-convert--restore-callout-list-element (car remaining)) body))
                (setq first nil end (plist-get (car remaining) :doc-end) remaining (cdr remaining)))
              (let ((separator (gdocs-convert--consume-generated-separator remaining end)))
                (setq remaining (plist-get separator :remaining) end (plist-get separator :end)))
              (setq body (nreverse body))
              (push (append (list :type 'callout-block :callout-type type :title title :body-elements body
                                  :paragraphs (mapcar (lambda (paragraph) (plist-get paragraph :contents)) body)
                                  :id (gdocs-convert--next-id))
                            (when start (list :doc-start start)) (when end (list :doc-end end))) result))
          (push element result) (setq remaining (cdr remaining)))))
    (nreverse result)))

(defun gdocs-convert--callout-style-base (type)
  "Return logical callout style family for TYPE."
  (intern (format "gdocs-callout-%s" type)))

(defun gdocs-convert--callout-label-style (type)
  "Return logical callout label style for TYPE."
  (intern (format "gdocs-callout-%s-label" type)))

(defun gdocs-convert--callout-label (type title)
  "Return visible callout label for TYPE and TITLE."
  (let ((label (or (cdr (assoc type gdocs-callout-labels)) (capitalize type))))
    (if (and title (not (string-empty-p title))) (format "%s — %s" label title) label)))

(defun gdocs-convert--callout-marker-data-string (element)
  "Encode callout ELEMENT type and UTF-8 title."
  (format "%s:%s" (or (plist-get element :callout-type) "info") (url-hexify-string (or (plist-get element :title) ""))))

(defun gdocs-convert--callout-list-marker-data-string (paragraph)
  "Encode callout PARAGRAPH list identity, not visible task completion."
  (let* ((info (plist-get paragraph :list))
         (data (list (cons 'type (symbol-name (or (plist-get info :type) 'bullet)))
                     (cons 'level (or (plist-get info :level) 0)))))
    ;; Task completion belongs to the visible glyph, never this snapshot.
    (dolist (property (unless (eq (plist-get info :type) 'check) '(:checked :partial :counter)))
      (when (plist-member info property)
        (setq data (append data (list (cons (intern (substring (symbol-name property) 1))
                                            (if (eq property :counter) (plist-get info property)
                                              (if (plist-get info property) 1 0))))))))
    (concat "v2:" (url-hexify-string (json-serialize data)))))

(defun gdocs-convert--prefix-runs (prefix runs)
  "Return RUNS with PREFIX prepended to the first run."
  (if runs (let ((first (copy-sequence (car runs))))
             (plist-put first :text (concat prefix (plist-get first :text))) (cons first (cdr runs)))
    (list (gdocs-convert--make-plain-run prefix))))

(defun gdocs-convert--callout-render-list-paragraph (paragraph)
  "Return native-list PARAGRAPH without injecting visible marker text."
  (copy-sequence paragraph))

(defun gdocs-convert--callout-paragraph-element (element role body index count)
  "Build callout ELEMENT ROLE paragraph from BODY at INDEX of COUNT."
  (let* ((type (or (plist-get element :callout-type) "info"))
         (style (if (eq role 'label) (gdocs-convert--callout-label-style type)
                  (gdocs-convert--block-line-style-name (gdocs-convert--callout-style-base type) index count)))
         (source (if (and (listp body) (plist-get body :type)) (copy-sequence body)
                   (gdocs-convert-make-paragraph body :style 'normal)))
         (info (plist-get source :list))
         (paragraph (if (and (eq role 'body) info) (gdocs-convert--callout-render-list-paragraph source) source))
         (markers (list (list :type (if (eq role 'label) 'callout-label 'callout-body)
                              :data (gdocs-convert--callout-marker-data-string element)))))
    (when (eq role 'label)
      (push (list :type 'callout-label-base
                  :data (url-hexify-string (gdocs-convert--callout-label type nil))) markers))
    (when (and (eq role 'body) info)
      (push (list :type 'callout-list :data (gdocs-convert--callout-list-marker-data-string source)) markers))
    (plist-put paragraph :style 'normal) (plist-put paragraph :style-name style)
    (dolist (marker markers)
      (let ((name (format "gdocs-org-marker:%s:%s:%s" (plist-get marker :type)
                          (plist-get element :id) (plist-get marker :data))))
        (when (> (gdocs-convert--string-to-utf16-length name) 256)
          (user-error "Callout metadata exceeds the native named-range limit; shorten the title or label"))))
    (plist-put paragraph :gdocs-marker (nreverse markers)) (plist-put paragraph :id (plist-get element :id))
    (gdocs-convert-task-project-paragraph paragraph)))

(defun gdocs-convert--callout-list-indent-reset-request (start end)
  "Return post-bullet full-width shading repair from START to END."
  `((updateParagraphStyle . ((paragraphStyle . ((indentStart . ((magnitude . 0) (unit . "PT")))
                                                (indentFirstLine . ((magnitude . 0) (unit . "PT")))))
                             (range . ((startIndex . ,start) (endIndex . ,(1- end))))
                             (fields . "indentStart,indentFirstLine")))))

(defun gdocs-convert--callout-block-to-requests (element index)
  "Plan callout ELEMENT at INDEX, including label, body, chrome and native bullets."
  (let* ((type (or (plist-get element :callout-type) "info")) (title (plist-get element :title))
         (body (gdocs-convert--callout-body-elements-from-paragraphs element)) (count (length body))
         (body-index 0) requests ranges (current index)
         (label (gdocs-convert--callout-paragraph-element element 'label
                                                          (list (gdocs-convert--make-plain-run (gdocs-convert--callout-label type title))) 0 1))
         (label-plan (gdocs-convert--paragraph-to-requests label current)))
    (setq requests (nconc requests (plist-get label-plan :requests)) current (plist-get label-plan :index))
    (dolist (item body)
      (let* ((paragraph (gdocs-convert--callout-paragraph-element element 'body item body-index count))
             (start current) (plan (gdocs-convert--paragraph-to-requests paragraph current)))
        (push (list :element paragraph :start start :end (plist-get plan :index)) ranges)
        (setq requests (nconc requests (plist-get plan :requests)) current (plist-get plan :index))
        (cl-incf body-index)))
    (let ((separator (gdocs-convert--append-generated-separator-requests requests current element)))
      (setq requests (plist-get separator :requests) current (plist-get separator :index)))
    (setq ranges (nreverse ranges) requests (gdocs-convert--fixup-list-nesting requests ranges))
    (list :requests requests :index (- current (apply #'+ (mapcar (lambda (range)
                                                                    (gdocs-convert--list-prefix-length-for-element (plist-get range :element))) ranges))))))

(defun gdocs-convert--callout-list-post-bullet-indent-resets (element-ranges)
  "Return feature-owned final shading indent resets for ELEMENT-RANGES."
  (let (requests)
    (dolist (range element-ranges (nreverse requests))
      (let* ((element (plist-get range :element)) (tabs (gdocs-convert--list-prefix-length-for-element element)))
        (when (and (plist-get element :list) (zerop tabs) (gdocs-convert--callout-list-marker-p element))
          (let* ((removed (gdocs-convert--list-prefixes-before range element-ranges))
                 (start (- (plist-get range :start) removed)) (end (- (plist-get range :end) removed tabs)))
            (push (gdocs-convert--callout-list-indent-reset-request start end) requests)))))))

(provide 'gdocs-convert-callouts)
;;; gdocs-convert-callouts.el ends here
