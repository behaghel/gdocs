;;; gdocs-convert-tables.el --- Table conversion and request geometry -*- lexical-binding: t; -*-

;;; Commentary:
;; Own source/native table semantics, cells, column geometry and request plans.
;; Generic inline/paragraph codecs supply cell content.  Document ID allocation
;; and authored-run styling remain shared document services.  No transport,
;; account selection or asynchronous orchestration belongs here.

;;; Code:

(require 'cl-lib)
(require 'org-element)
(require 'seq)
(require 'gdocs-convert-paragraph-codec)

(declare-function gdocs-convert--next-id "gdocs-convert" ())
(declare-function gdocs-convert--make-text-style-requests "gdocs-convert" (runs start-index))
(declare-function gdocs-convert--make-text-style-reset-request "gdocs-convert" (start end))

(defun gdocs-convert--table-to-ir (table)
  "Convert Org TABLE to table IR."
  (list :type 'table :rows (gdocs-convert--table-rows table) :id (gdocs-convert--next-id)))

(defun gdocs-convert--table-rows (table)
  "Decode ordinary rows of Org TABLE, excluding horizontal rules."
  (let (rows)
    (dolist (row (org-element-contents table))
      (when (and (eq (org-element-type row) 'table-row)
                 (eq (org-element-property :type row) 'standard))
        (push (gdocs-convert--table-row-cells row) rows)))
    (nreverse rows)))

(defun gdocs-convert--table-row-cells (row)
  "Decode Org table ROW into lists of shared inline runs."
  (let (cells)
    (dolist (cell (org-element-contents row))
      (when (eq (org-element-type cell) 'table-cell)
        (push (gdocs-convert--objects-to-runs (org-element-contents cell)) cells)))
    (nreverse cells)))

(defun gdocs-convert--ir-table-to-org (element)
  "Render table ELEMENT as Org, preserving the existing header separator rule."
  (let* ((rows (plist-get element :rows))
         (formatted (mapcar #'gdocs-convert--format-table-row rows)) parts)
    (when formatted
      (push (car formatted) parts)
      (when (cdr formatted)
        (push (gdocs-convert--table-separator (car rows)) parts)
        (dolist (row (cdr formatted)) (push row parts))))
    (mapconcat #'identity (nreverse parts) "\n")))

(defun gdocs-convert--format-table-row (row)
  "Render table ROW using shared inline Org rendering."
  (concat "| " (mapconcat #'gdocs-convert--runs-to-org row " | ") " |"))

(defun gdocs-convert--table-separator (row)
  "Render a separator matching ROW's column count."
  (concat "|" (mapconcat #'identity (make-list (length row) "---") "+") "|"))

(defun gdocs-convert--docs-table-to-ir (table)
  "Decode native TABLE into table IR."
  (let ((rows (seq-map #'gdocs-convert--docs-table-row-to-ir (alist-get 'tableRows table))))
    (list :type 'table :rows (seq-into rows 'list) :id (gdocs-convert--next-id))))

(defun gdocs-convert--docs-table-row-to-ir (row)
  "Decode native table ROW into cell run lists."
  (seq-into (seq-map #'gdocs-convert--docs-table-cell-to-runs (alist-get 'tableCells row)) 'list))

(defun gdocs-convert--docs-table-cell-to-runs (cell)
  "Decode native CELL paragraphs with the shared paragraph decoder."
  (let (runs)
    (seq-doseq (element (alist-get 'content cell))
      (when-let* ((paragraph (alist-get 'paragraph element)))
        (setq runs (nconc runs (gdocs-convert-paragraph-docs-runs paragraph nil)))))
    runs))

(defconst gdocs-convert--table-page-width-pt 468
  "Legacy usable page width in points for proportional table columns.")
(defconst gdocs-convert--table-min-column-width-pt 36
  "Legacy minimum readable column width in points.")
(defconst gdocs-convert--table-insert-overhead 3
  "Insertion overhead: preserved paragraph, table start and table end.
The read-side table body counts only the latter two structural units.")
(defconst gdocs-convert--table-base-cell-offset 4
  "Offset from insertion point to the first cell content.
Includes preserved paragraph, table start, row start and cell start.")

(defun gdocs-convert--table-to-requests (element index)
  "Plan table ELEMENT at INDEX as :requests and the following :index."
  (let* ((rows (plist-get element :rows)) (nrows (length rows)) (ncols (length (car rows)))
         (insert `((insertTable . ((rows . ,nrows) (columns . ,ncols) (location . ((index . ,index)))))))
         (length (+ gdocs-convert--table-insert-overhead (* nrows (+ 1 (* ncols 2)))
                    (gdocs-convert--table-total-cell-text-length rows)))
         (widths (gdocs-convert--table-column-widths (gdocs-convert--table-column-max-lengths rows)))
         (width-requests (gdocs-convert--table-column-width-requests (1+ index) ncols widths)))
    (list :requests (append (list insert) width-requests (gdocs-convert--table-cell-requests rows index ncols))
          :index (+ index length))))

(defun gdocs-convert--table-cell-requests (rows table-index ncols)
  "Plan ROWS at TABLE-INDEX with NCOLS, last cell first for stable insertion.
Each nonempty cell emits insert, inherited-format reset, then authored styles."
  (let (requests (r 0))
    (dolist (row rows)
      (let ((c 0))
        (dolist (cell row)
          (let* ((text (gdocs-convert--runs-to-plain-text cell))
                 (length (gdocs-convert--string-to-utf16-length text)))
            (when (> length 0)
              (let ((start (+ table-index gdocs-convert--table-base-cell-offset
                              (* r (+ 1 (* ncols 2))) (* c 2))))
                (dolist (style (reverse (gdocs-convert--make-text-style-requests cell start)))
                  (push style requests))
                (push (gdocs-convert--make-text-style-reset-request start (+ start length)) requests)
                (push (gdocs-convert--make-insert-text-request text start) requests))))
          (setq c (1+ c))))
      (setq r (1+ r)))
    requests))

(defun gdocs-convert--table-total-cell-text-length (rows)
  "Return total cell-text UTF-16 length in ROWS."
  (let ((total 0))
    (dolist (row rows)
      (dolist (cell row)
        (setq total (+ total (gdocs-convert--string-to-utf16-length
                              (gdocs-convert--runs-to-plain-text cell))))))
    total))

(defun gdocs-convert-table-body-length (element)
  "Return native read-side length of table ELEMENT, excluding preserved paragraph."
  (let ((total 2))
    (dolist (row (plist-get element :rows))
      (setq total (1+ total))
      (dolist (cell row)
        (setq total (+ total 2 (gdocs-convert--string-to-utf16-length
                                (gdocs-convert--runs-to-plain-text cell))))))
    total))

(defun gdocs-convert--table-column-max-lengths (rows)
  "Return longest character count per column in ROWS as a visual width proxy."
  (let* ((ncols (length (car rows))) (maxes (make-list ncols 0)))
    (dolist (row rows)
      (let ((c 0))
        (dolist (cell row)
          (when (< c ncols)
            (let ((length (length (gdocs-convert--runs-to-plain-text cell))))
              (when (> length (nth c maxes)) (setf (nth c maxes) length))))
          (setq c (1+ c)))))
    maxes))

(defun gdocs-convert--table-column-widths (max-lengths)
  "Distribute page width over MAX-LENGTHS, retaining minimum-width constraints."
  (let* ((ncols (length max-lengths)) (page-width (float gdocs-convert--table-page-width-pt))
         (min-width (min (float gdocs-convert--table-min-column-width-pt) (/ page-width ncols)))
         (total (apply #'+ max-lengths)))
    (if (zerop total) (make-list ncols (/ page-width ncols))
      (let* ((proportional (mapcar (lambda (length) (* (/ (float length) total) page-width)) max-lengths))
             (min-count 0) (remaining-total 0.0))
        (dolist (width proportional)
          (if (< width min-width) (setq min-count (1+ min-count))
            (setq remaining-total (+ remaining-total width))))
        (let ((remaining-width (- page-width (* min-count min-width))))
          (if (<= remaining-total 0) (make-list ncols (/ page-width ncols))
            (mapcar (lambda (width) (if (< width min-width) min-width
                                      (* (/ width remaining-total) remaining-width))) proportional)))))))

(defun gdocs-convert--table-column-width-requests (table-start-index ncols widths)
  "Plan WIDTHS for NCOLS at the actual native TABLE-START-INDEX."
  (let (requests)
    (dotimes (c ncols)
      (push `((updateTableColumnProperties
               . ((tableStartLocation . ((index . ,table-start-index)))
                  (columnIndices . ,(vector c))
                  (tableColumnProperties . ((widthType . "FIXED_WIDTH")
                                            (width . ((magnitude . ,(nth c widths)) (unit . "PT")))))
                  (fields . "*")))) requests))
    (nreverse requests)))

(defun gdocs-convert-table-plain-text (element)
  "Extract table ELEMENT text for comment matching without choosing link policy."
  (mapconcat (lambda (row) (mapconcat #'gdocs-convert--runs-to-plain-text row " "))
             (plist-get element :rows) " "))

(defun gdocs-sync--ir-has-tables-p (ir)
  "Return non-nil if IR contains table elements."
  (cl-some (lambda (element) (eq (plist-get element :type) 'table)) ir))

(defun gdocs-sync--compute-table-width-fixup-requests (local-ir json)
  "Plan width repair from LOCAL-IR using fetched native indices in JSON."
  (let* ((content (alist-get 'content (alist-get 'body json)))
         (tables (cl-remove-if-not (lambda (element) (eq (plist-get element :type) 'table)) local-ir))
         (index 0) requests)
    (seq-doseq (element content)
      (when (alist-get 'table element)
        (let* ((start (alist-get 'startIndex element))
               (ncols (alist-get 'columns (alist-get 'table element)))
               (table (nth index tables)) (rows (when table (plist-get table :rows)))
               (local-ncols (when rows (length (car rows)))))
          (when (and table (= ncols local-ncols))
            (setq requests (append requests
                                   (gdocs-convert--table-column-width-requests
                                    start ncols (gdocs-convert--table-column-widths
                                                 (gdocs-convert--table-column-max-lengths rows)))))))
        (cl-incf index)))
    requests))

(provide 'gdocs-convert-tables)
;;; gdocs-convert-tables.el ends here
