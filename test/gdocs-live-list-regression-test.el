;;; gdocs-live-list-regression-test.el --- Recorded native list regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline regressions from a disposable real-API document.  No credentials or
;; network are used.  Document identity/revision are removed from the fixture.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gdocs-disposable-plan)

(defconst gdocs-live-list-test--fixtures
  (expand-file-name "fixtures/" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory of public synthetic source and recorded native fixtures.")

(defun gdocs-live-list-test--old ()
  "Read the recorded disposable document body."
  (let ((json-object-type 'alist) (json-array-type 'vector) (json-key-type 'symbol))
    (gdocs-disposable-plan--body
     (gdocs-convert-docs-json-to-ir
      (json-read-file (expand-file-name "live-list-after-remote.json" gdocs-live-list-test--fixtures))))))

(defun gdocs-live-list-test--new ()
  "Read the intended synthetic update source."
  (gdocs-convert-org-string-to-ir
   (gdocs-disposable-plan--read-source (expand-file-name "live-list-updated.org" gdocs-live-list-test--fixtures))))

(ert-deftest gdocs-live-list-mixed-description-remains-bullet ()
  "An explicit hyphen does not inherit numbered type from a mixed list container."
  (let ((element (seq-find (lambda (item) (string-prefix-p "Updated term" (gdocs-convert--runs-to-plain-text (plist-get item :contents))))
                           (gdocs-live-list-test--new))))
    (should (eq (plist-get (plist-get element :list) :type) 'bullet))))

(ert-deftest gdocs-live-list-nesting-rebuild-clears-native-membership-before-tabs ()
  "An existing native bullet must be cleared before inserting nesting tabs."
  (let* ((old (gdocs-live-list-test--old))
         (element (seq-find (lambda (item) (equal (gdocs-convert--runs-to-plain-text (plist-get item :contents)) "Child two")) old))
         (start (plist-get element :doc-start))
         (new (gdocs-live-list-test--new))
         ;; The capture itself exposes double encoding: native level two while
         ;; the old snapshot says one.  Exercise a real native two-to-three edit.
         (updated (seq-find (lambda (item) (equal (gdocs-convert--runs-to-plain-text (plist-get item :contents)) "Child two updated")) new))
         (_level (plist-put updated :list (plist-put (plist-get updated :list) :level 3)))
         (_marker (plist-put (car (plist-get updated :gdocs-marker)) :data "number:3"))
         (requests (gdocs-diff-generate (list element) (list updated) start))
         (insert-position (seq-position requests t (lambda (request _) (equal (alist-get 'text (alist-get 'insertText request)) "\t\t\tChild two updated"))))
         (delete-position (seq-position requests t (lambda (request _) (alist-get 'deleteContentRange request))))
         (clear-position (seq-position requests t (lambda (request _)
                                                    (let ((range (alist-get 'range (alist-get 'deleteParagraphBullets request))))
                                                      (and range (= (alist-get 'startIndex range) start) (= (alist-get 'endIndex range) (1+ start))))))))
    (should insert-position)
    (should clear-position)
    (should delete-position)
    (should (< clear-position delete-position))
    (should (< clear-position insert-position))))

(ert-deftest gdocs-live-list-nesting-rebuild-refreshes-level-marker ()
  "A structural edit publishes fresh list-state metadata at the final native range."
  (let ((requests (gdocs-diff-generate (gdocs-live-list-test--old) (gdocs-live-list-test--new))))
    (should (= 1 (seq-count (lambda (request)
                              (when-let* ((name (alist-get 'name (alist-get 'createNamedRange request))))
                                (and (string-prefix-p "gdocs-org-marker:list-state:" name) (string-suffix-p ":number:2" name)))) requests)))))

(ert-deftest gdocs-live-list-word-edit-replaces-old-state-annotation-once ()
  "Changed list text cannot leave a stale or duplicate list-state range."
  (let* ((old (seq-find (lambda (item) (equal (gdocs-convert--runs-to-plain-text (plist-get item :contents)) "Parent one"))
                        (gdocs-live-list-test--old)))
         (marker (seq-find (lambda (value) (eq (plist-get value :type) 'list-state)) (plist-get old :gdocs-marker)))
         (new (gdocs-convert-org-string-to-ir "1. Parent one edited\n"))
         (requests (gdocs-diff-generate (list old) new (plist-get old :doc-start))))
    (should (seq-some (lambda (request) (equal (alist-get 'namedRangeId (alist-get 'deleteNamedRange request)) (plist-get marker :range-id))) requests))
    (should (= 1 (seq-count (lambda (request) (alist-get 'createNamedRange request)) requests)))))

(ert-deftest gdocs-live-list-visible-nesting-wins-over-old-marker ()
  "Complete native list properties take precedence over an old nesting snapshot."
  (let* ((bullet '((listId . "numbers") (nestingLevel . 2)))
         (lists '((numbers . ((listProperties . ((nestingLevels . [((glyphType . "DECIMAL")) ((glyphType . "ALPHA")) ((glyphType . "ROMAN"))])))))))
         (markers '((:type list-state :data "number:1" :start 1 :end 5))))
    (should (= (plist-get (gdocs-convert--docs-bullet-to-list bullet lists markers 1 5) :level) 2))))

(ert-deftest gdocs-live-list-native-group-base-offset-is-semantic-depth ()
  "Level zero in a visually nested mixed-type native list is not document depth zero."
  (let ((lists '((numbers . ((listProperties . ((nestingLevels . [((glyphType . "DECIMAL")
                                                                   (indentFirstLine . ((magnitude . 54) (unit . "PT")))
                                                                   (indentStart . ((magnitude . 72) (unit . "PT"))))])))))))
        (markers '((:type list-state :data "number:5" :start 1 :end 5))))
    (should (= (plist-get (gdocs-convert--docs-bullet-to-list '((listId . "numbers")) lists markers 1 5) :level) 1))))

(ert-deftest gdocs-live-list-paragraph-presentation-cannot-overrule-native-depth ()
  "A paragraph presentation override cannot flatten known native nesting."
  (let ((lists '((numbers . ((listProperties . ((nestingLevels . [((glyphType . "DECIMAL"))
                                                                  ((glyphType . "ALPHA"))
                                                                  ((glyphType . "ROMAN"))]))))))))
    (should (= (plist-get (gdocs-convert--docs-bullet-to-list
                           '((listId . "numbers") (nestingLevel . 2)) lists nil 1 5
                           '((indentFirstLine . ((magnitude . 0) (unit . "PT")))
                             (indentStart . ((magnitude . 0) (unit . "PT"))))) :level) 2))))

(ert-deftest gdocs-live-list-prefixed-creation-does-not-double-indent ()
  "Leading tabs encode nesting; pre-indentation must not add another nesting step."
  (let* ((element '(:type paragraph :style normal :list (:type bullet :level 1) :contents ((:text "child"))))
         (requests (gdocs-convert--make-list-requests element 10 17))
         (style (alist-get 'paragraphStyle (alist-get 'updateParagraphStyle (car requests)))))
    (should (= (alist-get 'magnitude (alist-get 'indentFirstLine style)) 0))
    (should (= (alist-get 'magnitude (alist-get 'indentStart style)) 0))))

(ert-deftest gdocs-live-list-nesting-edit-rebuilds-containing-group-last ()
  "A child cannot join an existing parent list and retain its requested nesting."
  (let* ((old (list '(:type paragraph :style normal :list (:type number :level 0) :contents ((:text "P")))
                    '(:type paragraph :style normal :list (:type number :level 1) :contents ((:text "C")))))
         (new (copy-tree old)))
    (setf (plist-get (plist-get (cadr new) :list) :level) 2)
    (let* ((requests (gdocs-diff-generate old new))
           (last-bullet (car (last (seq-filter (lambda (request) (alist-get 'createParagraphBullets request)) requests))))
           (range (alist-get 'range (alist-get 'createParagraphBullets last-bullet))))
      (should (= (alist-get 'startIndex range) 1))
      (should (= (alist-get 'endIndex range) 7))
      (should (seq-some (lambda (request)
                          (let ((range (alist-get 'range (alist-get 'deleteParagraphBullets request))))
                            (and range (= (alist-get 'startIndex range) 1) (= (alist-get 'endIndex range) 5)))) requests))
      (let ((prefix-position (cl-position-if
                              (lambda (request) (equal (alist-get 'text (alist-get 'insertText request)) "\t\t")) requests))
            (bullet-position (cl-position-if
                              (lambda (request) (eq request last-bullet)) requests)))
        (should prefix-position)
        (should (< prefix-position bullet-position))))))

(ert-deftest gdocs-live-list-group-repair-keeps-unaffected-groups ()
  "Changing one group's depth does not rebuild another native list."
  (let* ((old (gdocs-convert-org-string-to-ir "1. P\n   1. Before\n   2. C\n\nProse\n\n- Unchanged\n"))
         (new (gdocs-convert-org-string-to-ir "1. P\n   1. Before\n      1. C\n\nProse\n\n- Unchanged\n"))
         (requests (gdocs-diff-generate old new))
         (last-bullet (car (last (seq-filter (lambda (request) (alist-get 'createParagraphBullets request)) requests)))))
    (should (= (alist-get 'startIndex (alist-get 'range (alist-get 'createParagraphBullets last-bullet))) 1))
    (should-not (seq-some (lambda (request)
                            (equal (alist-get 'bulletPreset (alist-get 'createParagraphBullets request)) "BULLET_DISC_CIRCLE_SQUARE")) requests))))

(ert-deftest gdocs-live-list-group-repair-reapplies-continuation-indentation ()
  "A temporary group bullet cannot remain on a continuation paragraph."
  (let* ((old (gdocs-convert-org-string-to-ir "1. P\n   1. Before\n   2. C\n\n   Continue\n2. Next\n"))
         (new (gdocs-convert-org-string-to-ir "1. P\n   1. Before\n      1. C\n\n   Continue\n2. Next\n"))
         (requests (gdocs-diff-generate old new))
         (bullet-position (cl-position-if (lambda (request) (alist-get 'createParagraphBullets request)) requests :from-end t))
         (following (nthcdr (1+ bullet-position) requests)))
    (should (seq-some (lambda (request) (alist-get 'deleteParagraphBullets request)) following))
    (should (seq-some (lambda (request) (alist-get 'updateParagraphStyle request)) following))))

(ert-deftest gdocs-live-list-neutral-after-plan-composes-after-body-edits ()
  "An opaque after-plan runs after ordinary body edits without feature inspection."
  (let* ((gdocs-diff-semantic-plan-functions
          (list (lambda (&rest _) '(:after (((opaque-test-request . ((value . 7)))))))))
         (old '((:type paragraph :style normal :contents ((:text "old")))))
         (new '((:type paragraph :style normal :contents ((:text "new")))))
         (requests (gdocs-diff-generate old new)))
    (should (equal (car (last requests)) '((opaque-test-request . ((value . 7))))))))

(ert-deftest gdocs-live-list-partial-body-plan-blocks-unsafe-regrouping ()
  "A declined deletion cannot move after-plan requests onto protected text."
  (let* ((old (gdocs-convert-org-string-to-ir "Protected prose.\n\n1. P\n   1. Before\n   2. C\n"))
         (new (gdocs-convert-org-string-to-ir "1. P\n   1. Before\n      1. C\n"))
         (operations (gdocs-diff-compute-operations old new))
         (accepted (seq-remove (lambda (op) (eq (plist-get op :op) 'delete)) operations)))
    (should (seq-some (lambda (op) (eq (plist-get op :op) 'delete)) operations))
    (should-error (gdocs-diff-generate-from-ops old new accepted) :type 'user-error)))

(defun gdocs-live-list-test--json (name)
  "Read sanitized native fixture NAME."
  (let ((json-object-type 'alist) (json-array-type 'vector) (json-key-type 'symbol))
    (json-read-file (expand-file-name name gdocs-live-list-test--fixtures))))

(ert-deftest gdocs-live-list-recorded-single-edit-versus-group-edit ()
  "Recorded API snapshots expose single-paragraph flattening, not metadata guesses."
  (dolist (case '(("live-list-minimal-create.json" (0 1))
                  ("live-list-minimal-single-edit.json" (0 0))
                  ("live-list-minimal-group-edit.json" (0 2))))
    (let* ((json (gdocs-live-list-test--json (car case)))
           (native (gdocs-disposable-plan--body (gdocs-convert-docs-json-to-ir json))))
      (should-not (alist-get 'revisionId json))
      (should (equal (mapcar (lambda (element) (plist-get (plist-get element :list) :level)) native)
                     (cadr case))))))

(defun gdocs-live-list-test--legacy-layout (ir)
  "Return cloned IR with legacy physical task spacing, not hidden completion."
  (let ((copy (copy-tree ir)))
    (dolist (element copy copy)
      (when (gdocs-convert-task-paragraph-p element)
        (plist-put element :managed-task-legacy-separator t))
      (when (eq (plist-get element :type) 'callout-block)
        (plist-put element :body-elements
                   (gdocs-live-list-test--legacy-layout
                    (gdocs-convert--callout-body-elements-from-paragraphs element)))))))

(ert-deftest gdocs-live-list-recorded-legacy-pull-and-layout-upgrade ()
  "Legacy captures retain semantics and request an explicit physical task upgrade."
  (dolist (case '(("live-list-corrected-create.json" "live-list-updated.org")
                  ("live-list-second-update.json" "live-list-second-update.org")))
    (let* ((json (gdocs-live-list-test--json (car case)))
           (source (gdocs-disposable-plan--read-source (expand-file-name (cadr case) gdocs-live-list-test--fixtures)))
           (expected (gdocs-convert-org-string-to-ir source))
           (native (gdocs-disposable-plan--body (gdocs-convert-docs-json-to-ir json)))
           (pulled (gdocs-convert-ir-to-org native)) parents)
      (should-not (alist-get 'revisionId json))
      (should (equal pulled (gdocs-convert-ir-to-org expected)))
      (should-not (gdocs-diff-generate native (gdocs-live-list-test--legacy-layout expected)))
      (should-not (gdocs-diff-generate native (gdocs-live-list-test--legacy-layout expected)))
      (should (gdocs-diff-generate native expected))
      (should (gdocs-diff-generate native (gdocs-convert-org-string-to-ir pulled)))
      (seq-doseq (element (alist-get 'content (alist-get 'body json)))
        (let* ((paragraph (alist-get 'paragraph element))
               (text (mapconcat (lambda (run) (or (alist-get 'content (alist-get 'textRun run)) ""))
                                (alist-get 'elements paragraph) "")))
          (should-not (string-prefix-p "\t" text))
          (when (member text '("Parent one\n" "Parent two\n"))
            (push (alist-get 'listId (alist-get 'bullet paragraph)) parents)
            (should (= (or (alist-get 'nestingLevel (alist-get 'bullet paragraph)) 0) 0)))
          (when (string-prefix-p "Parent continuation" text)
            (should-not (alist-get 'bullet paragraph)))))
      (should (= (length parents) 2))
      (should (stringp (car parents)))
      (should (equal (car parents) (cadr parents))))))

(ert-deftest gdocs-live-list-styled-create-edit-pull-and-repeat ()
  "Themed real-API fixtures retain visible layout, authored semantics and no-op pushes."
  (dolist (case '(("live-list-styled-create.json" "live-list-styled-create.org")
                  ("live-list-styled-edited.json" "live-list-styled-edited.org")))
    (let* ((json (gdocs-live-list-test--json (car case)))
           (source (gdocs-disposable-plan--read-source
                    (expand-file-name (cadr case) gdocs-live-list-test--fixtures)))
           (expected (gdocs-convert-org-string-to-ir source))
           (native (gdocs-disposable-plan--body (gdocs-convert-docs-json-to-ir json)))
           (pulled (gdocs-convert-ir-to-org native)) (surfaces 0))
      (should (equal pulled (gdocs-convert-ir-to-org expected)))
      (should-not (gdocs-diff-generate native expected))
      (should-not (gdocs-diff-generate native expected))
      (should-not (gdocs-diff-generate native (gdocs-convert-org-string-to-ir pulled)))
      (seq-doseq (element (alist-get 'content (alist-get 'body json)))
        (let* ((paragraph (alist-get 'paragraph element))
               (style (alist-get 'paragraphStyle paragraph))
               (text (mapconcat (lambda (item) (or (alist-get 'content (alist-get 'textRun item)) ""))
                                (alist-get 'elements paragraph) "")))
          (when (alist-get 'shading style)
            (cl-incf surfaces)
            (let ((first (or (alist-get 'magnitude (alist-get 'indentFirstLine style)) 0))
                  (start (or (alist-get 'magnitude (alist-get 'indentStart style)) 0))
                  (end (or (alist-get 'magnitude (alist-get 'indentEnd style)) 0))
                  (left (or (alist-get 'magnitude (alist-get 'padding (alist-get 'borderLeft style))) 0))
                  (right (or (alist-get 'magnitude (alist-get 'padding (alist-get 'borderRight style))) 0)))
              (should (= (- (min first start) left) 0))
              (should (= (- end right) 0))))
          (when (string-match-p "Boundary gap retained" text)
            (should (= (alist-get 'magnitude (alist-get 'spaceAbove style)) 6)))
          (when (string-match-p "\\`[☐☑◩]\t" text)
            (should (= (- (alist-get 'magnitude (alist-get 'indentStart style))
                          (alist-get 'magnitude (alist-get 'indentFirstLine style))) 18)))))
      (should (> surfaces 5)))))

(provide 'gdocs-live-list-regression-test)
;;; gdocs-live-list-regression-test.el ends here
