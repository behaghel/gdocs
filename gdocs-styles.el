;;; gdocs-styles.el --- Configurable publishing presentation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pablo Stafforini

;;; Commentary:
;; Presentation data only: logical styles, fonts, colours, and visual distances.
;; This library loads without conversion, transport, accounts, or credentials.
;; Consumers override these values; conversion retains semantic identities.

;;; Code:

(require 'custom)

(defgroup gdocs-styles nil
  "Presentation configuration for Google Docs publishing."
  :group 'gdocs)

(defcustom gdocs-style-definitions
  '((gdocs-code :text (:monospace t))
    (gdocs-source-block :parent normal)
    (gdocs-source-block-first :parent normal)
    (gdocs-source-block-line :parent normal)
    (gdocs-source-block-last :parent normal)
    (gdocs-source-block-single :parent normal)
    (gdocs-quote-block :parent normal)
    (gdocs-quote-block-first :parent normal)
    (gdocs-quote-block-line :parent normal)
    (gdocs-quote-block-last :parent normal)
    (gdocs-quote-block-single :parent normal)
    (gdocs-callout-info-label :parent normal :text (:bold t))
    (gdocs-callout-note-label :parent normal :text (:bold t))
    (gdocs-callout-warning-label :parent normal :text (:bold t))
    (gdocs-callout-tip-label :parent normal :text (:bold t))
    (gdocs-callout-important-label :parent normal :text (:bold t))
    (gdocs-callout-info-first :parent normal)
    (gdocs-callout-info-line :parent normal)
    (gdocs-callout-info-last :parent normal)
    (gdocs-callout-info-single :parent normal)
    (gdocs-callout-note-first :parent normal)
    (gdocs-callout-note-line :parent normal)
    (gdocs-callout-note-last :parent normal)
    (gdocs-callout-note-single :parent normal)
    (gdocs-callout-warning-first :parent normal)
    (gdocs-callout-warning-line :parent normal)
    (gdocs-callout-warning-last :parent normal)
    (gdocs-callout-warning-single :parent normal)
    (gdocs-callout-tip-first :parent normal)
    (gdocs-callout-tip-line :parent normal)
    (gdocs-callout-tip-last :parent normal)
    (gdocs-callout-tip-single :parent normal)
    (gdocs-callout-important-first :parent normal)
    (gdocs-callout-important-line :parent normal)
    (gdocs-callout-important-last :parent normal)
    (gdocs-callout-important-single :parent normal)
    (gdocs-image-caption :parent normal))
  "Logical presentation styles, resolved when building Google Docs requests.
Each style has an optional :parent, :text, and :paragraph plist.  Text may set
:font-family, :font-size, :font-weight, :foreground-color, and
:background-color.
Paragraphs may set :space-above, :space-below, :indent-start, :indent-end,
:indent-first-line, :border-padding, and :borders.  Distances are in points.
These choices do not change Org or IR semantics."
  :type '(alist :key-type symbol :value-type plist)
  :group 'gdocs-styles)

(defcustom gdocs-style-base-font-size 11
  "Fallback font size in points used to resolve logical :font-scale values."
  :type 'number
  :group 'gdocs-styles)

(defcustom gdocs-convert-monospace-font-families
  '("Courier New" "Consolas" "Source Code Pro" "Roboto Mono" "Courier")
  "Fonts recognized as code on import; the first is the monospace fallback."
  :type '(repeat string)
  :group 'gdocs-styles)

(defcustom gdocs-style-default-border-color "#FFFFFF"
  "Fallback border colour as a hexadecimal #RRGGBB string."
  :type 'string
  :group 'gdocs-styles)

(defcustom gdocs-style-default-border-width 1
  "Fallback border width in points when a style omits :width."
  :type 'number
  :group 'gdocs-styles)

(defcustom gdocs-style-default-border-padding 0
  "Fallback border padding in points when a style omits :padding."
  :type 'number
  :group 'gdocs-styles)

(defvaralias 'gdocs-convert--status-default-styles 'gdocs-status-default-styles)

(defcustom gdocs-status-default-styles
  '((grey :bold t :foreground-color "#3C4043" :background-color "#E2E3E5")
    (red :bold t :foreground-color "#A50E0E" :background-color "#FCE8E6")
    (yellow :bold t :foreground-color "#7A4F01" :background-color "#FEF7E0")
    (green :bold t :foreground-color "#137333" :background-color "#E6F4EA")
    (blue :bold t :foreground-color "#174EA6" :background-color "#E8F0FE")
    (purple :bold t :foreground-color "#681DA8" :background-color "#F3E8FD"))
  "Fallback presentation for static status colour identifiers.
Each value is a logical text-style plist.  A gdocs-status-COLOR entry in
`gdocs-style-definitions' takes precedence.  Presentation does not alter
status colour identifiers, labels, original link spelling, or named ranges.
The former private palette variable remains an alias for compatibility."
  :type '(alist :key-type symbol :value-type plist)
  :group 'gdocs-styles)

(provide 'gdocs-styles)
;;; gdocs-styles.el ends here
