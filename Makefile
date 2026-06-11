EMACS ?= emacs
BATCH  = $(EMACS) -Q --batch -L .

.PHONY: all compile checkdoc test package-lint clean

all: compile checkdoc test

compile:
	$(BATCH) --eval "(setq byte-compile-error-on-warn t)" \
	         -f batch-byte-compile org-reading-list.el

checkdoc:
	$(BATCH) --eval "(checkdoc-file \"org-reading-list.el\")"

test:
	$(BATCH) -l tests/org-reading-list-tests.el \
	         -f ert-run-tests-batch-and-exit

# Installs package-lint from MELPA on first run (needs network).
package-lint:
	$(BATCH) --eval "(progn \
	    (require 'package) \
	    (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t) \
	    (package-initialize) \
	    (unless (package-installed-p 'package-lint) \
	      (package-refresh-contents) \
	      (package-install 'package-lint)))" \
	  -f package-lint-batch-and-exit org-reading-list.el

clean:
	rm -f *.elc
