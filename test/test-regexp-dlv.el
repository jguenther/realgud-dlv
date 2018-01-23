;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -L %s -L %s -L %s -l %s" (file-name-directory (locate-library "loc-changes.elc")) (file-name-directory (locate-library "load-relative.elc")) (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "realgud.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(require 'realgud)
(load-file "../dlv/init.el")
(load-file "./regexp-helper.el")

(setq backtrace-on-error t)

(declare-function __FILE__              'load-relative)

(test-simple-start)

(eval-when-compile
  (defvar dbg-name)   (defvar realgud-pat-hash)   (defvar realgud-bt-hash)
  (defvar loc-pat)    (defvar prompt-pat)
  (defvar file-group) (defvar line-group)         (defvar test-pos)
  (defvar test-dbgr)  (defvar test-text)
)

; Some setup usually done in setting up the buffer.
; We customize this for this debugger.
; FIXME: encapsulate this.
(setq dbg-name "dlv")

(setq loc-pat (gethash "loc" (gethash dbg-name realgud-pat-hash)))
(setq test-dbgr (make-realgud-cmdbuf-info
                  :debugger-name dbg-name
                  :loc-regexp (realgud-loc-pat-regexp loc-pat)
                  :file-group (realgud-loc-pat-file-group loc-pat)
                  :line-group (realgud-loc-pat-line-group loc-pat)))

;; FIXME: we get a void variable somewhere in here when running
;;        even though we define it in lexical-let. Dunno why.
;;        setq however will workaround this.
;; (setq test-text "/home/rocky/c/ctest.c:80:2000:beg:0x8048748>")
;; (note "traceback location matching")

;; (assert-t (numberp (cmdbuf-loc-match test-text test-dbgr)) "basic location")
;; (assert-equal "/home/rocky/c/ctest.c"
;;            (match-string (realgud-cmdbuf-info-file-group test-dbgr)
;;                          test-text) "extract file name")
;; (assert-equal "80"
;;            (match-string (realgud-cmdbuf-info-line-group test-dbgr)
;;                          test-text) "extract line number")
(note "debugger-backtrace")
(setq realgud-bt-pat  (gethash "debugger-backtrace"
                            realgud:dlv-pat-hash))
(setq test-text
      "0  0x000000000132ad69 in path/and/package.function
   at ./main_test.go:44
1  0x000000000110a31f in testing.tRunner
   at /usr/local/Cellar/go/1.9.2/libexec/src/testing/testing.go:746
2  0x0000000001061e41 in runtime.goexit
   at /usr/local/Cellar/go/1.9.2/libexec/src/runtime/asm_amd64.s:2337
")
(setq realgud-bt-re (realgud-loc-pat-regexp realgud-bt-pat))
(setq file-group (realgud-loc-pat-file-group realgud-bt-pat))
(setq line-group (realgud-loc-pat-line-group realgud-bt-pat))
(assert-equal 0 (string-match realgud-bt-re test-text))

(assert-equal "main_test.go"
              (substring test-text
                         (match-beginning file-group)
                         (match-end file-group)))
(assert-equal "44"
              (substring test-text
                         (match-beginning line-group)
                         (match-end line-group)))

(setq test-pos (match-end 0))
(assert-equal 74 test-pos)
(assert-equal 74 (string-match realgud-bt-re test-text test-pos))
(assert-equal "/usr/local/Cellar/go/1.9.2/libexec/src/testing/testing.go"
              (substring test-text
                         (match-beginning file-group)
                         (match-end file-group)))
(assert-equal "746"
              (substring test-text
                         (match-beginning line-group)
                         (match-end line-group)))

(setq test-pos (match-end 0))
(assert-equal 183 test-pos)
(assert-equal 183 (string-match realgud-bt-re test-text test-pos))
(assert-equal "/usr/local/Cellar/go/1.9.2/libexec/src/runtime/asm_amd64.s"
              (substring test-text
                         (match-beginning file-group)
                         (match-end file-group)))
(assert-equal "2337"
              (substring test-text
                         (match-beginning line-group)
                         (match-end line-group)))

(note "prompt")
(set (make-local-variable 'prompt-pat)
     (gethash "prompt" realgud:dlv-pat-hash))
(prompt-match "(dlv) ")

(end-tests)
