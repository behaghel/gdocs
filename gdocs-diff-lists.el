;;; gdocs-diff-lists.el --- List paragraph diff policy -*- lexical-binding: t; -*-

;;; Commentary:
;; List identity, membership, nesting prefixes and native bullet lifecycle.
;; The generic diff kernel consumes a neutral paragraph descriptor and finisher.

;;; Code:

(require 'cl-lib)
(require 'gdocs-convert-lists)
(defvar gdocs-diff-semantic-plan-functions)
(declare-function gdocs-convert--make-marker-requests "gdocs-convert" (element start end))
(declare-function gdocs-diff--marker-key "gdocs-diff" (element))
(declare-function gdocs-diff--element-utf16-length "gdocs-diff" (element))
(declare-function gdocs-diff-compute-operations "gdocs-diff" (old-ir new-ir))

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
     (if (plist-get (gdocs-diff-lists-paragraph new old) :compatible)
         (plist-put result :insert-reqs
                    (cl-remove-if (lambda (request) (alist-get 'createParagraphBullets request))
                                  (plist-get result :insert-reqs)))
       (when (plist-get old :list)
         ;; Clear membership while the old paragraph still contains text.
         ;; A newline-only range can leave Google retaining the old nesting;
         ;; then a later create consumes tabs without rebuilding membership.
         (let ((start (car range)))
           (plist-put result :delete-reqs
                      (append (list (gdocs-diff--make-delete-bullets-request start (+ start 2))
                                    `((updateParagraphStyle
                                       . ((paragraphStyle . ((indentStart . ((magnitude . 0) (unit . "PT")))
                                                             (indentFirstLine . ((magnitude . 0) (unit . "PT")))))
                                          (range . ((startIndex . ,start) (endIndex . ,(1+ start))))
                                          (fields . "indentStart,indentFirstLine")))))
                              (plist-get result :delete-reqs))))))
     (when (plist-get new :list)
       (let* ((copy (copy-sequence new))
              (end (+ (car range) (gdocs-convert--string-to-utf16-length
                                   (gdocs-convert--runs-to-plain-text (plist-get new :contents))) 1)))
         (plist-put copy :gdocs-marker
                    (seq-filter (lambda (marker) (eq (plist-get marker :type) 'list-state))
                                (plist-get new :gdocs-marker)))
         ;; Native bullet creation has now consumed structural tabs.  Attach
         ;; fresh metadata at final physical coordinates, never the old extent.
         (plist-put result :insert-reqs
                    (append (plist-get result :insert-reqs)
                            (gdocs-convert--make-marker-requests copy (car range) end))))))
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

(defun gdocs-diff-lists--membership (element)
  "Return ELEMENT's native membership, excluding managed glyph tasks."
  (when-let* ((info (or (plist-get element :list)
                        (gdocs-convert-list-continuation-native-group element))))
    (list (if (plist-get element :list) 'item 'continuation)
          (plist-get info :type) (or (plist-get info :level) 0))))

(defun gdocs-diff-lists--final-ranges (new start)
  "Compute NEW's final physical ranges from START, not old document coordinates."
  (let ((index 0) ranges)
    (dolist (element new (nreverse ranges))
      (let ((end (+ start (gdocs-diff--element-utf16-length element))))
        (push (list :element element :index index :start start :end end) ranges)
        (setq start end index (1+ index))))))

(defun gdocs-diff-lists--changed-indices (old new operations)
  "Find NEW indices whose native groups OPERATIONS structurally change from OLD."
  (let ((cursor 0) indices)
    (dolist (operation operations (delete-dups indices))
      (let* ((kind (plist-get operation :op))
             (i (plist-get operation :new-index))
             (before (when (memq kind '(modify delete))
                       (gdocs-diff-lists--membership (nth (plist-get operation :old-index) old))))
             (after (when i (gdocs-diff-lists--membership (nth i new)))))
        (cond
         ((and (eq kind 'modify) (not (equal before after)))
          (push i indices)
          (unless after (push (1- i) indices) (push (1+ i) indices)))
         ((and (eq kind 'insert) after) (push i indices))
         ((and (eq kind 'delete) before)
          (push cursor indices) (push (1- cursor) indices)))
        (when i (setq cursor (1+ i)))))))

(defun gdocs-diff-lists--rebuild-group (group ranges)
  "Rebuild native GROUP at final RANGES without replacing authored text or styles."
  (let ((start (plist-get group :start)) (end (plist-get group :end))
        (prefixes 0) prefixed insertions)
    (dolist (range ranges)
      (let* ((element (plist-get range :element))
             (prefix (gdocs-convert--list-leading-prefix element))
             (length (length prefix)))
        (when (> length 0)
          ;; Descending insertion indices keep every final body coordinate valid.
          (push `((insertText . ((text . ,prefix) (location . ((index . ,(plist-get range :start))))))) insertions))
        (push (list :element element :start (+ (plist-get range :start) prefixes)
                    :end (+ (plist-get range :end) prefixes length)) prefixed)
        (setq prefixes (+ prefixes length))))
    (setq prefixed (nreverse prefixed))
    (append
     (list `((deleteParagraphBullets . ((range . ((startIndex . ,start) (endIndex . ,end))))))
           (gdocs-convert--make-list-indent-request 0 start end t))
     insertions
     (list `((createParagraphBullets
              . ((range . ((startIndex . ,start) (endIndex . ,(+ end prefixes))))
                 (bulletPreset . ,(plist-get group :preset))))))
     (gdocs-convert--callout-list-post-bullet-indent-resets prefixed)
     (gdocs-convert-list-continuation-post-bullet-indents prefixed))))

(defun gdocs-diff-lists--regroup (old new operations start)
  "Regroup native lists after OLD to NEW OPERATIONS at START."
  (when-let* ((indices (gdocs-diff-lists--changed-indices old new operations)))
    ;; Final coordinates describe the complete new body.  Never apply them to
    ;; a partial push that retained declined/comment-protected old elements.
    (dolist (expected (gdocs-diff-compute-operations old new))
      (when (and (not (eq (plist-get expected :op) 'keep))
                 (not (member expected operations)))
        (user-error "Native list regrouping needs a complete body plan; push nesting changes separately from declined or comment-protected edits")))
    (let* ((ranges (gdocs-diff-lists--final-ranges new start))
           (groups (gdocs-convert--find-list-groups ranges)) requests)
      (gdocs-convert--alternate-numbered-presets groups)
      (dolist (group (reverse groups) requests)
        (let ((members (seq-filter (lambda (range)
                                     (and (>= (plist-get range :start) (plist-get group :start))
                                          (<= (plist-get range :end) (plist-get group :end)))) ranges)))
          (when (seq-some (lambda (range) (memq (plist-get range :index) indices)) members)
            ;; Recreating only a child joins the preceding native list at level
            ;; zero in the API.  Recreate its complete group after body edits.
            (setq requests (append requests (gdocs-diff-lists--rebuild-group group members)))))))))

(defun gdocs-diff-lists--plan (old new operations start)
  "Plan OLD to NEW list OPERATIONS and native repairs at START."
  (let (ids)
    (dolist (operation operations)
      (let* ((kind (plist-get operation :op))
             (element (when (memq kind '(modify delete)) (nth (plist-get operation :old-index) old)))
             (updated (when (eq kind 'modify) (nth (plist-get operation :new-index) new))))
        (when (and (listp element) (plist-get element :list)
                   (or (eq kind 'delete)
                       (not (equal (gdocs-convert--runs-to-plain-text (plist-get element :contents))
                                   (gdocs-convert--runs-to-plain-text (plist-get updated :contents))))
                       (not (plist-get (gdocs-diff-lists-paragraph updated element) :compatible))
                       (not (equal (gdocs-diff--marker-key element) (gdocs-diff--marker-key updated)))))
          (dolist (marker (plist-get element :gdocs-marker))
            (when (eq (plist-get marker :type) 'list-state)
              (push (plist-get marker :range-id) ids))))))
    (let ((after (gdocs-diff-lists--regroup old new operations start)))
      (when (or (delq nil ids) after)
        (list :before (mapcar (lambda (id) `((deleteNamedRange . ((namedRangeId . ,id)))))
                              (delete-dups (delq nil (nreverse ids))))
              :after after)))))

(add-hook 'gdocs-diff-semantic-plan-functions #'gdocs-diff-lists--plan t)

(provide 'gdocs-diff-lists)
;;; gdocs-diff-lists.el ends here
