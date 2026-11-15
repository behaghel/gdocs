EMACS ?= emacs
ELPACA_REPOS := $(dir $(CURDIR))
LOAD_PATH := -L $(CURDIR) -L test \
             -L $(ELPACA_REPOS)plz \
             -L $(ELPACA_REPOS)dash \
             -L $(ELPACA_REPOS)s \
             -L $(ELPACA_REPOS)transient/lisp \
             -L $(ELPACA_REPOS)cond-let \
             -L $(ELPACA_REPOS)seq \
             -L $(ELPACA_REPOS)llama \
             -L $(ELPACA_REPOS)compat

TEST_FILES = \
  test/gdocs-test-helpers.el \
  test/gdocs-auth-test.el \
  test/gdocs-api-test.el \
  test/gdocs-convert-test.el \
  test/gdocs-text-paragraphs-test.el \
  test/gdocs-paragraph-codec-test.el \
  test/gdocs-inline-codec-test.el \
  test/gdocs-convert-headings-test.el \
  test/gdocs-footnote-reference-seam-test.el \
  test/gdocs-convert-status-test.el \
  test/gdocs-styles-test.el \
  test/gdocs-style-resolve-test.el \
  test/gdocs-link-resolve-test.el \
  test/gdocs-table-convert-test.el \
  test/gdocs-image-convert-test.el \
  test/gdocs-code-blocks-test.el \
  test/gdocs-quotes-test.el \
  test/gdocs-fidelity-corrections-test.el \
  test/gdocs-diff-test.el \
  test/gdocs-diff-encapsulation-test.el \
  test/gdocs-diff-status-test.el \
  test/gdocs-sync-test.el \
  test/gdocs-pull-preservation-test.el \
  test/gdocs-sync-status-test.el \
  test/gdocs-merge-test.el \
  test/gdocs-roundtrip-test.el \
  test/gdocs-test.el

LOAD_TESTS = $(foreach f,$(TEST_FILES),-l $(f))

SRC_FILES := $(wildcard gdocs*.el)

.PHONY: test test-specimen test-verbose compile clean

test:
	$(EMACS) -Q --batch $(LOAD_PATH) -l ert $(LOAD_TESTS) \
	  -f ert-run-tests-batch-and-exit

# Named CI gate for the standard document specimens, also part of `test`.
test-specimen:
	$(EMACS) -Q --batch $(LOAD_PATH) -l ert -l test/gdocs-roundtrip-test.el \
	  --eval '(ert-run-tests-batch-and-exit "^gdocs-roundtrip/")'

test-verbose:
	$(EMACS) -Q --batch $(LOAD_PATH) -l ert $(LOAD_TESTS) \
	  --eval '(ert-run-tests-batch-and-exit t)'

compile:
	$(EMACS) -Q --batch $(LOAD_PATH) \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(SRC_FILES)

clean:
	rm -f *.elc test/*.elc
