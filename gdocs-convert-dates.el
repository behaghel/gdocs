;;; gdocs-convert-dates.el --- Native inline date semantics -*- lexical-binding: t; -*-

;;; Commentary:
;; Timestamp recognition, active/inactive markers and native model addressing
;; belong here rather than in shared inline conversion or insertion algorithms.

;;; Code:

(require 'org-element)
(require 'seq)
(require 'subr-x)
(require 'gdocs-convert-text)

(defcustom gdocs-convert-native-date-elements t
  "Whether Org timestamps should push as native Google Docs date elements."
  :type 'boolean :group 'gdocs)

(defun gdocs-convert-date-source-object (object props)
  "Decode timestamp OBJECT with inherited PROPS, or nil when unhandled."
  (when (eq (org-element-type object) 'timestamp)
    (gdocs-convert--timestamp-to-runs object props)))

(defun gdocs-convert--timestamp-to-runs (timestamp inherited-props)
  "Convert TIMESTAMP with INHERITED-PROPS to a native date run."
  (let ((raw (org-element-interpret-data timestamp))
        (year (org-element-property :year-start timestamp))
        (month (org-element-property :month-start timestamp))
        (day (org-element-property :day-start timestamp)))
    (list (append (gdocs-convert--make-run raw inherited-props)
                  (when (and gdocs-convert-native-date-elements year month day)
                    (list :date-element
                          (list :timestamp (format "%04d-%02d-%02dT00:00:00Z" year month day)
                                :raw raw :active (eq (org-element-property :type timestamp) 'active))))))))

(defun gdocs-convert-date-native-parts (element markers)
  "Decode native date ELEMENT using MARKERS, or nil when unhandled."
  (when-let* ((date (alist-get 'dateElement element)))
    (list (cons nil (gdocs-convert--docs-date-element-to-run
                     date markers (alist-get 'startIndex element) (alist-get 'endIndex element))))))

(defun gdocs-convert-date-run-policy (run)
  "Return native date addressing and atomicity policy for RUN, or nil."
  (when (plist-get run :date-element) '(:atomic t :model-length 1)))

(defun gdocs-convert--timestamp-kind-marker (markers start end)
  "Return timestamp kind marker overlapping START..END in MARKERS, or nil."
  (seq-find (lambda (marker)
              (and (eq (plist-get marker :type) 'timestamp-kind)
                   (let ((a (plist-get marker :start)) (b (plist-get marker :end)))
                     (and a b start end (< a end) (> b start))))) markers))

(defun gdocs-convert--timestamp-active-p (markers start end)
  "Return non-nil when timestamp marker at START..END in MARKERS is active."
  (not (equal (plist-get (gdocs-convert--timestamp-kind-marker markers start end) :data) "inactive")))

(defun gdocs-convert--docs-date-element-to-run (date-element &optional markers start end)
  "Convert DATE-ELEMENT with MARKERS at START..END to timestamp IR."
  (let* ((timestamp (alist-get 'timestamp (alist-get 'dateElementProperties date-element)))
         (active (gdocs-convert--timestamp-active-p markers start end))
         (raw (gdocs-convert--docs-date-timestamp-to-org timestamp (if active 'active 'inactive))))
    (list :text raw :date-element (list :timestamp timestamp :raw raw :active active))))

(defun gdocs-convert--docs-date-timestamp-to-org (timestamp &optional kind)
  "Convert native TIMESTAMP to Org with active/inactive KIND."
  (if (and timestamp (string-match "\\`\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)" timestamp))
      (let* ((year (string-to-number (match-string 1 timestamp)))
             (month (string-to-number (match-string 2 timestamp)))
             (day (string-to-number (match-string 3 timestamp)))
             (weekday (format-time-string "%a" (encode-time 0 0 0 day month year t) t)))
        (format "%s%04d-%02d-%02d %s%s" (if (eq kind 'inactive) "[" "<")
                year month day weekday (if (eq kind 'inactive) "]" ">")))
    (if (eq kind 'inactive) "[1970-01-01 Thu]" "<1970-01-01 Thu>")))

(defun gdocs-convert-date-inline-insert-requests (runs index prefix)
  "Plan RUNS containing native dates at INDEX with PREFIX, or nil when unhandled."
  (when (seq-some (lambda (run) (plist-get run :date-element)) runs)
    (let (requests (cursor index))
      (unless (string-empty-p prefix)
        (push (gdocs-convert--make-insert-text-request prefix cursor) requests)
        (setq cursor (+ cursor (gdocs-convert--string-to-utf16-length prefix))))
      (dolist (run runs)
        (if-let* ((date (plist-get run :date-element)))
            (progn
              (push (gdocs-convert--make-insert-date-request date cursor) requests)
              (unless (plist-get date :active)
                (push (gdocs-convert--make-timestamp-kind-marker-request date cursor) requests))
              (setq cursor (1+ cursor)))
          (let ((text (plist-get run :text)))
            (unless (string-empty-p text)
              (push (gdocs-convert--make-insert-text-request text cursor) requests)
              (setq cursor (+ cursor (gdocs-convert--string-to-utf16-length text)))))))
      (push (gdocs-convert--make-insert-text-request "\n" cursor) requests)
      (list :requests (nreverse requests) :length (1+ (- cursor index))))))

(defun gdocs-convert--make-timestamp-kind-marker-request (date index)
  "Create a named range marker for inactive DATE at INDEX."
  `((createNamedRange . ((name . ,(format "gdocs-org-marker:timestamp-kind:%s:%s" index
                                          (if (plist-get date :active) "active" "inactive")))
                         (range . ((startIndex . ,index) (endIndex . ,(1+ index))))))))

(defun gdocs-convert--make-insert-date-request (date index)
  "Create an insertDate request for DATE at INDEX."
  `((insertDate . ((dateElementProperties . ((timestamp . ,(plist-get date :timestamp))
                                             (locale . "en")
                                             (dateFormat . "DATE_FORMAT_MONTH_DAY_YEAR_ABBREVIATED")
                                             (timeFormat . "TIME_FORMAT_DISABLED")))
                   (location . ((index . ,index)))))))

(provide 'gdocs-convert-dates)
;;; gdocs-convert-dates.el ends here
