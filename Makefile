EMACS ?= emacs

.PHONY: test
test:
	$(EMACS) -Q --batch -l tests/meta-test.el -f ert-run-tests-batch-and-exit
