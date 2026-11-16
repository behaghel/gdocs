;;; gdocs-convert-list-publish.el --- Native list publication -*- lexical-binding: t; -*-

;;; Commentary:
;; Native bullet presets, nesting tabs, list groups and post-bullet coordinates.
;; Peer features own their own shading and managed-text indentation repairs.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'gdocs-convert-tasks)
(require 'gdocs-convert-text)
(defconst gdocs-convert--list-indent-pt 36
  "Points of indentation per native list nesting level.")

(defun gdocs-convert--list-type-to-preset (type)
  "Convert IR list TYPE to a native bullet preset, rejecting unprojected tasks."
  (pcase type
    ('bullet "BULLET_DISC_CIRCLE_SQUARE")
    ('number "NUMBERED_DECIMAL_ALPHA_ROMAN")
    ('check (user-error "Task lists require managed glyph projection before native list planning"))))
(declare-function gdocs-convert-list-continuation-native-group "gdocs-convert-lists" (element))
(declare-function gdocs-convert-list-continuation-post-bullet-indents "gdocs-convert-lists" (ranges))
(declare-function gdocs-convert--callout-list-post-bullet-indent-resets "gdocs-convert-callouts" (ranges))

(defun gdocs-convert--list-leading-prefix (element)
  "Return nesting tabs inserted for native list ELEMENT."
  (let* ((info (plist-get element :list)) (level (and info (or (plist-get info :level) 0))))
    (if (and level (> level 0)) (make-string level ?\t) "")))

(defun gdocs-convert--make-list-requests (element start end)
  "Create bullet/list requests for ELEMENT from START to END."
  (when-let* ((info (plist-get element :list)))
    (let* ((preset (gdocs-convert--list-type-to-preset (plist-get info :type)))
           (bullet `((createParagraphBullets . ((range . ((startIndex . ,start) (endIndex . ,(1- end))))
                                                (bulletPreset . ,preset)))))
           (prefix-length (length (gdocs-convert--list-leading-prefix element)))
           (body-length (gdocs-convert--string-to-utf16-length
                         (gdocs-convert--runs-to-plain-text (plist-get element :contents))))
           (prefixed (and (> prefix-length 0) (= (- end start 1) (+ body-length prefix-length))))
           ;; Tabs already encode the nesting level.  A pre-existing indent
           ;; adds another level in the real API; start from zero instead.
           (indent (when prefixed (gdocs-convert--make-list-indent-request 0 start end t))))
      (append (when indent (list indent)) (list bullet)))))

(defun gdocs-convert--make-list-indent-request (level start end &optional reset)
  "Set native indentation LEVEL from START to END.
With RESET, set zero indentation before tabs supply nesting during creation."
  (when (or reset (> level 0))
    (let ((indent-start (if reset 0 (* gdocs-convert--list-indent-pt (1+ level))))
          (indent-first (if reset 0 (+ (/ gdocs-convert--list-indent-pt 2) (* gdocs-convert--list-indent-pt level)))))
      `((updateParagraphStyle
         . ((paragraphStyle . ((indentStart . ((magnitude . ,indent-start) (unit . "PT")))
                               (indentFirstLine . ((magnitude . ,indent-first) (unit . "PT")))))
            (range . ((startIndex . ,start) (endIndex . ,(1- end))))
            (fields . "indentStart,indentFirstLine")))))))

(defconst gdocs-convert--numbered-presets '("NUMBERED_DECIMAL_ALPHA_ROMAN" "NUMBERED_DECIMAL_NESTED")
  "Equivalent level-zero presets used to keep consecutive numbered groups separate.")

(defun gdocs-convert--alternate-numbered-presets (groups)
  "Alternate equivalent presets between consecutive numbered GROUPS in place."
  (let (previous)
    (dolist (group groups)
      (let ((preset (plist-get group :preset)))
        (when (and (string= preset previous) (string-prefix-p "NUMBERED_" preset))
          (plist-put group :preset (gdocs-convert--next-numbered-preset preset)))
        (setq previous (plist-get group :preset))))))

(defun gdocs-convert--next-numbered-preset (preset)
  "Return the next equivalent numbered PRESET."
  (nth (% (1+ (or (seq-position gdocs-convert--numbered-presets preset) 0))
          (length gdocs-convert--numbered-presets)) gdocs-convert--numbered-presets))

(defun gdocs-convert--fixup-list-nesting (requests element-ranges)
  "Finalize native list REQUESTS using ELEMENT-RANGES.
Create contiguous groups last-to-first before peer-owned post-bullet repairs."
  (let ((groups (gdocs-convert--find-list-groups element-ranges)))
    (if (null groups) requests
      (let ((filtered (cl-remove-if
                       (lambda (request)
                         (and (alist-get 'createParagraphBullets request)
                              (gdocs-convert--bullet-request-in-list-groups-p request groups))) requests)))
        (gdocs-convert--alternate-numbered-presets groups)
        (append filtered (mapcar #'gdocs-convert--list-group-bullet-request (reverse groups))
                (gdocs-convert--callout-list-post-bullet-indent-resets element-ranges)
                (gdocs-convert--guard-non-list-paragraphs element-ranges groups)
                (gdocs-convert-task-post-bullet-indents element-ranges)
                (gdocs-convert-list-continuation-post-bullet-indents element-ranges))))))

(defun gdocs-convert--bullet-request-in-list-groups-p (request groups)
  "Return non-nil if bullet REQUEST belongs to one of GROUPS."
  (when-let* ((bullet (alist-get 'createParagraphBullets request)) (range (alist-get 'range bullet))
              (start (alist-get 'startIndex range)) (end (alist-get 'endIndex range)))
    (seq-some (lambda (group) (and (>= start (plist-get group :start))
                                   (<= end (1- (plist-get group :end))))) groups)))

(defun gdocs-convert--find-list-groups (element-ranges)
  "Return contiguous native list groups in ELEMENT-RANGES."
  (let (groups current-start current-end current-preset)
    (dolist (range element-ranges)
      (let* ((element (plist-get range :element))
             (info (or (plist-get element :list) (gdocs-convert-list-continuation-native-group element))))
        (if info
            (let ((preset (gdocs-convert--list-type-to-preset (plist-get info :type))))
              (if (and current-start (string= preset current-preset)) (setq current-end (plist-get range :end))
                (when current-start (push (list :start current-start :end current-end :preset current-preset) groups))
                (setq current-start (plist-get range :start) current-end (plist-get range :end) current-preset preset)))
          (when current-start
            (push (list :start current-start :end current-end :preset current-preset) groups)
            (setq current-start nil current-end nil current-preset nil)))))
    (when current-start (push (list :start current-start :end current-end :preset current-preset) groups))
    (nreverse groups)))

(defun gdocs-convert--list-prefix-length-for-element (element)
  "Return native nesting tab count inserted for ELEMENT."
  (if (plist-get element :list) (length (gdocs-convert--list-leading-prefix element)) 0))

(defun gdocs-convert--list-prefixes-before (range element-ranges)
  "Return count of removed list tabs before RANGE in ELEMENT-RANGES."
  (let ((start (plist-get range :start)) (total 0))
    (dolist (candidate element-ranges total)
      (when (< (plist-get candidate :start) start)
        (setq total (+ total (gdocs-convert--list-prefix-length-for-element (plist-get candidate :element))))))))

(defun gdocs-convert--list-group-bullet-request (group)
  "Create the native bullet request for GROUP."
  `((createParagraphBullets . ((range . ((startIndex . ,(plist-get group :start))
                                         (endIndex . ,(1- (plist-get group :end)))))
                               (bulletPreset . ,(plist-get group :preset))))))

(defun gdocs-convert--guard-non-list-paragraphs (element-ranges groups)
  "Clear accidentally inherited bullets around GROUPS using ELEMENT-RANGES."
  (when groups
    (let ((gaps (gdocs-convert--list-group-gaps element-ranges groups)) requests)
      (dolist (range element-ranges)
        (let ((element (plist-get range :element)) (start (plist-get range :start)) (end (plist-get range :end)))
          (when (and (eq (plist-get element :type) 'paragraph) (not (plist-get element :list))
                     (gdocs-convert--in-any-gap-p start end gaps))
            (let* ((removed (gdocs-convert--list-prefixes-before range element-ranges))
                   (a (- start removed)) (b (- end removed)))
              (when (< a b)
                (push `((deleteParagraphBullets . ((range . ((startIndex . ,a) (endIndex . ,(1- b))))))) requests))))))
      (nreverse requests))))

(defun gdocs-convert--list-group-gaps (element-ranges groups)
  "Compute gaps around and between GROUPS within ELEMENT-RANGES."
  (let (gaps)
    (cl-loop for (first second) on groups while second
             do (push (cons (plist-get first :end) (plist-get second :start)) gaps))
    (gdocs-convert--add-edge-gaps gaps (car groups) (car (last groups)) element-ranges)))

(defun gdocs-convert--add-edge-gaps (gaps first-group last-group ranges)
  "Add document-edge GAPS around FIRST-GROUP and LAST-GROUP in RANGES."
  (let ((start (plist-get (car ranges) :start)) (end (plist-get (car (last ranges)) :end)))
    (when (> (plist-get first-group :start) start) (push (cons start (plist-get first-group :start)) gaps))
    (when (< (plist-get last-group :end) end) (push (cons (plist-get last-group :end) end) gaps))
    gaps))

(defun gdocs-convert--in-any-gap-p (start end gaps)
  "Return non-nil when START..END is contained in GAPS."
  (cl-some (lambda (gap) (and (>= start (car gap)) (<= end (cdr gap)))) gaps))

(provide 'gdocs-convert-list-publish)
;;; gdocs-convert-list-publish.el ends here
