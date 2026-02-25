EMACS ?= emacs

.PHONY: test lint clean

test:
	$(EMACS) -Q -batch -L . -l test/org-github-test.el -f ert-run-tests-batch-and-exit

lint:
	$(EMACS) -Q -batch -L . \
	  --eval "(require 'package)" \
	  --eval "(package-initialize)" \
	  --eval "(require 'package-lint)" \
	  -f package-lint-batch-and-exit org-github.el

clean:
	rm -f *.elc test/*.elc
