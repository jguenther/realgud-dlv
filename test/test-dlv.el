;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -L %s -L %s -L %s -l %s" (file-name-directory (locate-library "loc-changes.elc")) (file-name-directory (locate-library "load-relative.elc")) (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "realgud.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(load-file "../dlv/core.el")
(load-file "./regexp-helper.el")

(eval-when-compile
  (defvar realgud:dlv-minibuffer-history)
  (defvar test:realgud-dlv-executable-save)
  (defvar test:realgud-minibuffer-history-save)
)

(declare-function realgud:dlv-suggest-invocation 'realgud:bashdb)
(declare-function __FILE__              'require-relative)

(test-simple-start)

;; Save value realgud:run-process and change it to something we want
(setq test:realgud-dlv-executable-save (symbol-function 'realgud:dlv-executable))
(setq test:realgud-minibuffer-history-save realgud:dlv-minibuffer-history)

(defun realgud:dlv-executable (filename)
  "Mock function for testing"
  (cond ((equal filename "bar.sh") 7)
        ((equal filename "foo") 8)
        ((equal filename "baz") 8)
        (t 3)))

(defun dlv-test()
  (note "realgud:dlv-suggest-invocation")
  (setq realgud:dlv-minibuffer-history nil)
  (let ((my-directory (file-name-directory (__FILE__))))
    (save-excursion
      (note "Test preference to buffer editing")
      (setq default-directory
            (concat my-directory "dlv"))
      (find-file-literally "foo.go")
      (assert-equal "dlv debug foo" (realgud:dlv-suggest-invocation)
                    "Should find file sans extension - foo")
      (find-file-literally "baz.c")
      (assert-equal "dlv debug baz" (realgud:dlv-suggest-invocation)
                    "Should find file sans extension - baz")
      )
    (save-excursion
      (note "Pick up non-sans executable")
      (setq default-directory
            (concat my-directory  "dlv/test2"))
      ;; (assert-equal "dlv bar.sh" (realgud:dlv-suggest-invocation))
      (setq realgud:dlv-minibuffer-history '("dlv testing"))
      (setq default-directory
            (concat my-directory  "dlv/test2"))
      (assert-equal "dlv testing" (realgud:dlv-suggest-invocation)
                    "After setting minibuffer history - takes precidence")
      )
    (setq default-directory my-directory)
    )
  )
(dlv-test)
(end-tests)

;; Restore the old values.
;; You might have to run the below if you run this interactively.
(fset 'realgud:dlv-executable test:realgud-dlv-executable-save)
(setq realgud:dlv-minibuffer-history test:realgud-minibuffer-history-save)
(setq default-directory (file-name-directory (__FILE__)))
