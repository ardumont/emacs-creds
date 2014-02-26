;; from shell: emacs -Q --batch -l ./launch-tests.el

(let ((current-directory (expand-file-name ".")))
  (load-file (concat current-directory "/" "creds.el"))
  (load-file (concat current-directory "/" "creds-tests.el")))

(require 'creds)
(require 'creds-tests)
(ert-run-tests-batch-and-exit)
