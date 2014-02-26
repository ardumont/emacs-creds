init:
	cask init

deps:
	cask

tests:
	cask exec emacs -Q -batch \
			-l ert \
			-l ./launch-tests.el \
			-f ert-run-tests-batch-and-exit
