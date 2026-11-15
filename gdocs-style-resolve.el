;;; gdocs-style-resolve.el --- Resolve logical publishing presentation -*- lexical-binding: t; -*-

;;; Commentary:
;; Translate configured presentation data into native style fields and logical
;; style requests.  No Org parsing, semantic feature recognition, diffing,
;; account selection or transport belongs here.  Legacy helper names remain
;; available through the conversion facade.

;;; Code:

(require 'subr-x)
(require 'gdocs-styles)
(require 'gdocs-convert-text)

(defun gdocs-style-resolve-find-definition (style-name)
  "Return STYLE-NAME's first configured entry, including an explicitly empty one."
  (assq style-name gdocs-style-definitions))

(defun gdocs-convert--style-definition (style-name)
  "Return logical style definition for STYLE-NAME."
  (cdr (gdocs-style-resolve-find-definition style-name)))

(defun gdocs-convert--ir-alignment-to-docs (alignment)
  "Convert an IR ALIGNMENT symbol to a Google Docs alignment string."
  (pcase alignment ('center "CENTER") ('end "END") ('justified "JUSTIFIED") (_ nil)))

(defun gdocs-convert--ir-style-to-docs (style)
  "Translate logical parent STYLE to a native named-style enum.
Unknown symbolic parents retain the normal-text fallback.  This is presentation
configuration translation, not document heading recognition."
  (or (cdr (assoc (symbol-name style)
                  '(("heading-1" . "HEADING_1") ("heading-2" . "HEADING_2")
                    ("heading-3" . "HEADING_3") ("heading-4" . "HEADING_4")
                    ("heading-5" . "HEADING_5") ("heading-6" . "HEADING_6")
                    ("title" . "TITLE") ("subtitle" . "SUBTITLE"))))
      "NORMAL_TEXT"))

(defun gdocs-convert--hex-to-color (hex)
  "Convert HEX color string (#RRGGBB) to a Google Docs color alist."
  (let ((r (/ (string-to-number (substring hex 1 3) 16) 255.0))
        (g (/ (string-to-number (substring hex 3 5) 16) 255.0))
        (b (/ (string-to-number (substring hex 5 7) 16) 255.0)))
    `((color . ((rgbColor . ((red . ,r) (green . ,g) (blue . ,b))))))))

(defun gdocs-convert--logical-text-style-to-docs (style-plist)
  "Resolve logical text STYLE-PLIST to Google Docs TextStyle fields."
  (let ((style nil))
    (let ((font-family (or (plist-get style-plist :font-family)
                           (when (plist-get style-plist :monospace)
                             (car gdocs-convert-monospace-font-families))))
          (font-weight (plist-get style-plist :font-weight)))
      (when font-family
        (push `(weightedFontFamily
                . ,(append `((fontFamily . ,font-family))
                           (when font-weight `((weight . ,font-weight))))) style)))
    (when (plist-member style-plist :bold) (push `(bold . ,(plist-get style-plist :bold)) style))
    (when (plist-member style-plist :italic) (push `(italic . ,(plist-get style-plist :italic)) style))
    (when (plist-member style-plist :underline) (push `(underline . ,(plist-get style-plist :underline)) style))
    (when-let ((fg (plist-get style-plist :foreground-color)))
      (push `(foregroundColor . ,(gdocs-convert--hex-to-color fg)) style))
    (when-let ((bg (plist-get style-plist :background-color)))
      (push `(backgroundColor . ,(gdocs-convert--hex-to-color bg)) style))
    (when-let ((scale (plist-get style-plist :font-scale)))
      (push `(fontSize . ((magnitude . ,(* gdocs-style-base-font-size scale)) (unit . "PT"))) style))
    (when-let ((size (plist-get style-plist :font-size)))
      (push `(fontSize . ((magnitude . ,size) (unit . "PT"))) style))
    style))

(defun gdocs-convert--logical-paragraph-style-to-docs (style-plist)
  "Resolve logical paragraph STYLE-PLIST to Google Docs ParagraphStyle fields."
  (let ((style nil))
    (when-let ((alignment (plist-get style-plist :alignment)))
      (push `(alignment . ,(gdocs-convert--ir-alignment-to-docs alignment)) style))
    (when-let ((spacing-mode (plist-get style-plist :spacing-mode)))
      (push `(spacingMode . ,(pcase spacing-mode
                               ('never-collapse "NEVER_COLLAPSE") ('collapse-lists "COLLAPSE_LISTS") (_ nil))) style))
    (when-let ((space-above (plist-get style-plist :space-above)))
      (push `(spaceAbove . ((magnitude . ,space-above) (unit . "PT"))) style))
    (when-let ((space-below (plist-get style-plist :space-below)))
      (push `(spaceBelow . ((magnitude . ,space-below) (unit . "PT"))) style))
    (when-let ((indent-start (plist-get style-plist :indent-start)))
      (push `(indentStart . ((magnitude . ,indent-start) (unit . "PT"))) style))
    (when (plist-member style-plist :indent-first-line)
      (push `(indentFirstLine . ((magnitude . ,(plist-get style-plist :indent-first-line)) (unit . "PT"))) style))
    (when-let ((indent-end (plist-get style-plist :indent-end)))
      (push `(indentEnd . ((magnitude . ,indent-end) (unit . "PT"))) style))
    (if-let ((borders (plist-get style-plist :borders)))
        (setq style (append (gdocs-convert--paragraph-border-styles borders style-plist) style))
      (when-let ((padding (plist-get style-plist :border-padding)))
        (setq style (append (gdocs-convert--paragraph-padding-border-style
                             padding (or (plist-get style-plist :border-color)
                                         (plist-get style-plist :background-color)
                                         gdocs-style-default-border-color)) style))))
    (when-let ((background (plist-get style-plist :background-color)))
      (push `(shading . ((backgroundColor . ,(gdocs-convert--hex-to-color background)))) style))
    (delq nil style)))

(defun gdocs-convert--paragraph-border-object (spec fallback-color fallback-padding)
  "Resolve border SPEC using FALLBACK-COLOR and FALLBACK-PADDING when absent."
  `((color . ,(gdocs-convert--hex-to-color
               (or (plist-get spec :color) fallback-color gdocs-style-default-border-color)))
    (width . ((magnitude . ,(or (plist-get spec :width) gdocs-style-default-border-width)) (unit . "PT")))
    (padding . ((magnitude . ,(or (plist-get spec :padding) fallback-padding
                                  gdocs-style-default-border-padding)) (unit . "PT")))
    (dashStyle . "SOLID")))

(defun gdocs-convert--paragraph-border-field (side)
  "Return Google Docs paragraph border field for SIDE."
  (pcase side ('top 'borderTop) ('bottom 'borderBottom) ('left 'borderLeft) ('right 'borderRight)))

(defun gdocs-convert--paragraph-border-styles (borders style-plist)
  "Resolve BORDERS using fallback attributes in STYLE-PLIST."
  (let ((color (or (plist-get style-plist :border-color)
                   (plist-get style-plist :background-color) gdocs-style-default-border-color))
        (padding (plist-get style-plist :border-padding)))
    (mapcar (lambda (entry) (cons (gdocs-convert--paragraph-border-field (car entry))
                                  (gdocs-convert--paragraph-border-object (cdr entry) color padding))) borders)))

(defun gdocs-convert--paragraph-padding-border-style (padding color)
  "Return paragraph borders with PADDING in points and COLOR."
  (let ((border (gdocs-convert--paragraph-border-object
                 (list :color color :width gdocs-style-default-border-width :padding padding) color padding)))
    `((borderTop . ,border) (borderBottom . ,border) (borderLeft . ,border) (borderRight . ,border))))

(defun gdocs-convert--style-field-mask-name (field)
  "Return Google Docs field mask name for STYLE FIELD."
  (pcase field ('shading "shading.backgroundColor") (_ (symbol-name field))))

(defun gdocs-convert--style-fields-mask-from-alist (style)
  "Return comma-separated native field mask for STYLE alist."
  (mapconcat (lambda (pair) (gdocs-convert--style-field-mask-name (car pair))) style ","))

(defun gdocs-convert--style-fields-mask (style-fields)
  "Compute the native fields mask for STYLE-FIELDS alist."
  (mapconcat (lambda (pair) (symbol-name (car pair))) style-fields ","))

(defun gdocs-convert--logical-style-paragraph-request (style-name start end)
  "Return logical STYLE-NAME paragraph request between START and END."
  (when-let* ((definition (gdocs-convert--style-definition style-name)))
    (let* ((parent (plist-get definition :parent))
           (paragraph-style (gdocs-convert--logical-paragraph-style-to-docs (plist-get definition :paragraph)))
           (named-style (and parent (gdocs-convert--ir-style-to-docs parent)))
           (style (append (when named-style `((namedStyleType . ,named-style))) paragraph-style)))
      (when style
        `((updateParagraphStyle . ((paragraphStyle . ,style)
                                   (range . ((startIndex . ,start) (endIndex . ,(1- end))))
                                   (fields . ,(gdocs-convert--style-fields-mask-from-alist style)))))))))

(defun gdocs-convert--logical-style-text-fields (style-name)
  "Return native text fields for logical STYLE-NAME."
  (gdocs-convert--logical-text-style-to-docs (plist-get (gdocs-convert--style-definition style-name) :text)))

(defun gdocs-convert--logical-style-text-request (style-name start end &optional segment-id)
  "Return logical STYLE-NAME text request from START to END in optional SEGMENT-ID."
  (let ((style (gdocs-convert--logical-style-text-fields style-name)))
    (when style
      `((updateTextStyle . ((textStyle . ,style)
                            (range . ,(gdocs-convert--text-range start end segment-id))
                            (fields . ,(gdocs-convert--style-fields-mask-from-alist style))))))))

(defun gdocs-convert-make-logical-style-requests
    (style-name start-index end-index &optional text-start text-end segment-id)
  "Create paragraph/text requests for logical STYLE-NAME.
START-INDEX and END-INDEX target paragraph style in the body.  When
TEXT-START and TEXT-END are non-nil, also create text style requests.
Optional SEGMENT-ID targets the text style request at a non-body segment."
  (delq nil (list (gdocs-convert--logical-style-paragraph-request style-name start-index end-index)
                  (when (and text-start text-end)
                    (gdocs-convert--logical-style-text-request style-name text-start text-end segment-id)))))

(provide 'gdocs-style-resolve)
;;; gdocs-style-resolve.el ends here
