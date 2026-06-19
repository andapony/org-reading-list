EMACS ?= emacs
BATCH  = $(EMACS) -Q --batch -L .

# Every Emacs Lisp source we ship, so compilation catches problems
# (missing lexical-binding cookies, warnings) in any of them, not just
# the main file.  Generated artifacts (*-pkg.el, *-autoloads.el) are not
# listed here; they are produced by the package manager at install time.
EL = org-reading-list.el org-reading-list-mi.el $(wildcard tests/*.el)

.PHONY: all compile checkdoc test package-lint clean

all: compile checkdoc test

compile:
	$(BATCH) --eval "(setq byte-compile-error-on-warn t)" \
	         -f batch-byte-compile $(EL)

checkdoc:
	$(BATCH) --eval "(checkdoc-file \"org-reading-list.el\")"
	$(BATCH) --eval "(checkdoc-file \"org-reading-list-mi.el\")"

test:
	$(BATCH) -l tests/org-reading-list-tests.el \
	         -l tests/org-reading-list-mi-tests.el \
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
	rm -f *.elc tests/*.elc
