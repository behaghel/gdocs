;;; gdocs-convert-headings-test.el --- Heading conversion contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; Source/import heading levels, metadata, canonicalization, and round trips.
;; Cross-document heading-link resolution remains in the general link tests.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gdocs-convert)

(defun gdocs-convert-test--render-docs-heading (level elements)
  "Render a fetched heading at LEVEL with JSON paragraph ELEMENTS."
  (gdocs-convert-ir-to-org
   (gdocs-convert-docs-json-to-ir
    (json-parse-string
     (format (concat "{\"body\":{\"content\":[{\"paragraph\":{"
                     "\"paragraphStyle\":{\"namedStyleType\":\"HEADING_%d\"},"
                     "\"elements\":[%s]}}]}}")
             level elements)
     :object-type 'alist :false-object :json-false))))

(ert-deftest gdocs-headings-conversion-has-a-dedicated-module ()
  "Heading conversion and normalization live together outside the dispatcher."
  (dolist (function '(gdocs-convert--headline-to-ir gdocs-convert--format-heading
                                                    gdocs-convert--normalize-heading-bold))
    (should (equal (file-name-base (symbol-file function 'defun))
                   "gdocs-convert-headings"))))

(ert-deftest gdocs-convert-test-heading-levels ()
  "Heading levels 1-6 map to heading-1 through heading-6."
  (dolist (level '(1 2 3 4 5 6))
    (let* ((stars (make-string level ?*))
           (org-str (format "%s Heading %d" stars level))
           (ir (gdocs-convert-org-string-to-ir org-str))
           (elem (car ir)))
      (should (eq (plist-get elem :style) (intern (format "heading-%d" level))))
      (should (eq (plist-get elem :style-name) (intern (format "gdocs-heading-%d" level)))))))

(ert-deftest gdocs-convert-test-docs-headings-do-not-wrap-titles-in-bold ()
  "Fetched heading presentation does not create whole-title Org bold markup."
  (dolist (level '(1 2 3 4 5 6))
    (let* ((elements (format (concat "{\"textRun\":{\"content\":\"Section %d\\n\","
                                     "\"textStyle\":{\"bold\":%s}}}")
                             level (if (>= level 3) "true" "false")))
           (result (gdocs-convert-test--render-docs-heading level elements)))
      (should (equal result (format "%s Section %d\n" (make-string level ?*) level))))))

(ert-deftest gdocs-convert-test-docs-headings-preserve-contrasting-italic ()
  "Removing whole-heading bold retains independently visible italic text."
  (let ((elements
         (concat "{\"textRun\":{\"content\":\"Alpha \",\"textStyle\":{\"bold\":true}}},"
                 "{\"textRun\":{\"content\":\"careful\",\"textStyle\":{\"bold\":true,\"italic\":true}}},"
                 "{\"textRun\":{\"content\":\" heading\\n\",\"textStyle\":{\"bold\":true}}}")))
    (should (equal (gdocs-convert-test--render-docs-heading 3 elements)
                   "*** Alpha /careful/ heading\n"))))

(ert-deftest gdocs-convert-test-docs-headings-preserve-partial-bold ()
  "Bold contrasting with the rest of a fetched heading remains authored markup."
  (let ((elements
         (concat "{\"textRun\":{\"content\":\"Alpha \",\"textStyle\":{\"bold\":false}}},"
                 "{\"textRun\":{\"content\":\"careful\",\"textStyle\":{\"bold\":true}}},"
                 "{\"textRun\":{\"content\":\" heading\\n\",\"textStyle\":{\"bold\":false}}}")))
    (should (equal (gdocs-convert-test--render-docs-heading 3 elements)
                   "*** Alpha *careful* heading\n"))))

(ert-deftest gdocs-convert-test-docs-body-keeps-uniform-bold ()
  "Whole-paragraph bold in normal body text remains semantic Org emphasis."
  (let* ((json (json-parse-string
                (concat "{\"body\":{\"content\":[{\"paragraph\":{"
                        "\"paragraphStyle\":{\"namedStyleType\":\"NORMAL_TEXT\"},"
                        "\"elements\":[{\"textRun\":{\"content\":\"Bold body.\\n\","
                        "\"textStyle\":{\"bold\":true}}}]}}]}}")
                :object-type 'alist))
         (result (gdocs-convert-ir-to-org (gdocs-convert-docs-json-to-ir json))))
    (should (equal result "*Bold body.*\n"))))

(ert-deftest gdocs-convert-test-heading-level-7-clamped ()
  "Heading level 7+ is clamped to heading-6."
  (let* ((ir (gdocs-convert-org-string-to-ir "******* Deep heading"))
         (elem (car ir)))
    (should (eq (plist-get elem :style) 'heading-6))))

(ert-deftest gdocs-convert-test-docs-json-heading ()
  "A Google Docs HEADING_1 converts to :style heading-1."
  (let* ((json `((body . ((content
                           . [((paragraph
                                . ((elements
                                    . [((textRun . ((content . "Title\n") (textStyle . ()))) )])
                                   (paragraphStyle . ((namedStyleType . "HEADING_1"))))))])))))
         (ir (gdocs-convert-docs-json-to-ir json))
         (heading (car ir)))
    (should (eq (plist-get heading :style) 'heading-1))
    (should (eq (plist-get heading :style-name) 'gdocs-heading-1))))

(ert-deftest gdocs-convert-test-docs-json-preserves-heading-hierarchy ()
  "Pulled Google Docs headings retain their Org hierarchy."
  (cl-labels ((para (text style)
                `((paragraph
                   . ((elements . [((textRun . ((content . ,(concat text "\n"))
                                                (textStyle . ()))) )])
                      (paragraphStyle . ((namedStyleType . ,style))))))))
    (let* ((json `((body . ((content . ,(vector
                                         (para "Smoke protocol" "HEADING_1")
                                         (para "Expected validation flow" "HEADING_2")
                                         (para "High-value visual checks" "HEADING_2")
                                         (para "Executive summary" "HEADING_1")))))))
           (org (gdocs-convert-ir-to-org (gdocs-convert-docs-json-to-ir json))))
      (should (string-match-p "^\\* Smoke protocol$" org))
      (should (string-match-p "^\\*\\* Expected validation flow$" org))
      (should (string-match-p "^\\*\\* High-value visual checks$" org))
      (should (string-match-p "^\\* Executive summary$" org))
      (should-not (string-match-p "^\\* Smoke protocol\\(?:.\\|\n\\)*^\\* Smoke protocol$" org)))))

(ert-deftest gdocs-convert-test-todo-marker ()
  "TODO keywords are stored in :gdocs-marker as a list of plists."
  (let* ((ir (gdocs-convert-org-string-to-ir "* TODO Task"))
         (marker (plist-get (car ir) :gdocs-marker)))
    (should marker)
    (should (listp marker))
    (should (eq (plist-get (car marker) :type) 'todo))
    (should (string= (plist-get (car marker) :data) "TODO"))))

(ert-deftest gdocs-convert-test-tags-marker ()
  "Tags are stored in :gdocs-marker as a list of plists."
  (let* ((ir (gdocs-convert-org-string-to-ir "* Heading :tag1:tag2:"))
         (marker (plist-get (car ir) :gdocs-marker)))
    (should marker)
    (should (listp marker))
    (should (eq (plist-get (car marker) :type) 'tags))
    (should (string= (plist-get (car marker) :data) "tag1:tag2"))))

(ert-deftest gdocs-convert-test-todo-round-trip ()
  "TODO keywords survive round-trip via markers."
  (let* ((ir (gdocs-convert-org-string-to-ir "* TODO Important task\n"))
         (result (gdocs-convert-ir-to-org ir)))
    (should (string-match-p "\\* TODO Important task" result))))

(ert-deftest gdocs-convert-test-tags-round-trip ()
  "Tags survive round-trip via markers."
  (let* ((ir (gdocs-convert-org-string-to-ir "* Heading :work:urgent:\n"))
         (result (gdocs-convert-ir-to-org ir)))
    (should (string-match-p ":work:urgent:" result))))

(ert-deftest gdocs-convert-test-priority-marker ()
  "A heading with priority produces a marker with :type priority."
  (let* ((ir (gdocs-convert-org-string-to-ir "* [#A] Urgent task"))
         (marker (plist-get (car ir) :gdocs-marker)))
    (should marker)
    (should (listp marker))
    (should (eq (plist-get (car marker) :type) 'priority))
    (should (string= (plist-get (car marker) :data) "A"))))

(ert-deftest gdocs-convert-test-scheduled-marker ()
  "SCHEDULED produces a marker with :type scheduled."
  (let* ((ir (gdocs-convert-org-string-to-ir "* Task\nSCHEDULED: <2026-03-15>"))
         (marker (plist-get (car ir) :gdocs-marker)))
    (should marker)
    (should (listp marker))
    (should (eq (plist-get (car marker) :type) 'scheduled))
    (should (string-match-p "2026-03-15" (plist-get (car marker) :data)))))

(ert-deftest gdocs-convert-test-deadline-marker ()
  "DEADLINE produces a marker with :type deadline."
  (let* ((ir (gdocs-convert-org-string-to-ir "* Task\nDEADLINE: <2026-03-20>"))
         (marker (plist-get (car ir) :gdocs-marker)))
    (should marker)
    (should (listp marker))
    (should (eq (plist-get (car marker) :type) 'deadline))
    (should (string-match-p "2026-03-20" (plist-get (car marker) :data)))))

(ert-deftest gdocs-convert-test-round-trip-headings-and-nested-lists ()
  "Headings and nested lists survive round-trip."
  (let* ((original "* Top heading\n\n- Parent item\n  - Child item\n    - Grandchild item\n\n** Sub heading\n\nParagraph.\n")
         (result (gdocs-convert-ir-to-org (gdocs-convert-org-string-to-ir original))))
    (should (string-match-p "\\* Top heading" result))
    (should (string-match-p "\\*\\* Sub heading" result))
    (should (string-match-p "- Parent item" result))
    (should (string-match-p "  - Child item" result))
    (should (string-match-p "    - Grandchild item" result))
    (should (string-match-p "Paragraph\\." result))))

(ert-deftest gdocs-convert-test-ir-to-org-keeps-blank-after-heading-before-list ()
  "A section that starts with a list keeps a blank line after its heading."
  (let ((ir '((:type paragraph :style heading-1 :contents ((:text "Section")))
              (:type paragraph :style normal :list (:type bullet :level 0)
                     :contents ((:text "First item"))))))
    (should (equal (gdocs-convert-ir-to-org ir) "* Section\n\n- First item\n"))))

(ert-deftest gdocs-convert-test-round-trip-multiple-headings ()
  "Multiple headings at different levels survive round-trip."
  (let* ((original "* Heading one\n\n** Heading two\n\n*** Heading three\n")
         (result (gdocs-convert-ir-to-org (gdocs-convert-org-string-to-ir original))))
    (should (string= (string-trim result) (string-trim original)))))

(provide 'gdocs-convert-headings-test)
;;; gdocs-convert-headings-test.el ends here
