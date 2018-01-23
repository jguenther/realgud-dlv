;; Copyright (C) 2016-2017 Rocky Bernstein

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; dlv debugger

(eval-when-compile (require 'cl-lib))

(require 'realgud)

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat (realgud-loc))

(defvar realgud:dlv-pat-hash (make-hash-table :test 'equal)
  "hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  the values of a hash entry is a
realgud-loc-pat struct")

(declare-function make-realgud-loc "realgud-loc" (a b c d e f))

(defconst realgud:dlv-frame-file-regexp
  (format "\\(.+\\):%s" realgud:regexp-captured-num))

(defconst realgud:dlv-frame-start-regexp
  "\\(?:^\\|\n\\)")

;; Some versions of dlv insert "frame" and some don't.
(defconst realgud:dlv-frame-num-regexp
  (format "%s +"
          realgud:regexp-captured-num))

; (setf (gethash "loc-callback-fn" realgud:dlv-pat-hash) 'realgud:dlv-loc-fn-callback)

;; realgud-loc-pat that describes a dlv location generally shown
;; before a command prompt.
;; For example:
;; * thread #1: tid = 12866, 0x00000000004004b4 hello`main(argc=1, argv=0x00007fffffffd668) + 4 at hello.c:5, name = 'hello', stop reason = breakpoint 1.1
(setf (gethash "loc" realgud:dlv-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^> _.* %s ("
                       realgud:dlv-frame-file-regexp)
       :file-group 1
       :line-group 2))

;; Top frame number
(setf (gethash "top-frame-num" realgud:dlv-pat-hash) 0)

;; realgud-loc-pat that describes a dlv frame generally shown
;; before a command prompt or in frame switching commands
;;  frame #1: 0x00000000004015e2 ctest`main(argc=1, argv=0x00007fffffffd778) + 90 at ctest.c:83
;; Some versions of dlv give:
;; #0  main (argc=2, argv=0xbffff564, envp=0xbffff570) at main.c:935
;; instead

(setf (gethash "selected-frame" realgud:dlv-pat-hash)
      (make-realgud-loc-pat
       :regexp          (format "^%s.* at %s"
                        realgud:dlv-frame-num-regexp
                        realgud:dlv-frame-file-regexp
                        )
       :num 1
       :file-group 2
       :line-group 3)
      )

;; realgud-loc-pat that describes a dlv prompt
;; For example:
;;   (dlv)
(setf (gethash "prompt" realgud:dlv-pat-hash)
      (make-realgud-loc-pat
       :regexp   "^(dlv) "
       ))

;; realgud-loc-pat that describes a "breakpoint set" line
;; For example:
;;   Breakpoint 1 set at 0x132ac1f for _/file/path/here.Function_name() ./main_test.go:42
(setf (gethash "brkpt-set" realgud:dlv-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^Breakpoint %s set at 0x[a-f0-9]* for .* \\(.+\\):%s\n"
                       realgud:regexp-captured-num realgud:regexp-captured-num)
       :num 1
       :file-group 2
       :line-group 3))

;; realgud-loc-pat that describes a "breakpoint set" line
;; For example:
;;   Breakpoint 1 cleared at 0x132ac1f for _/file/path/here.Function_name() ./main_test.go:42
(setf (gethash "brkpt-del" realgud:dlv-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^Breakpoint %s cleared at 0x[a-f0-9]* for .* \\(.+\\):%s\n"
                       realgud:regexp-captured-num realgud:regexp-captured-num)
       :num 1
       :file-group 2
       :line-group 3))


;; realgud-loc-pat that describes a dlv "backtrace" command line.
;; For example:
;; #0  main (argc=2, argv=0xbffff564, envp=0xbffff570) at main.c:935
;; #1  0xb7e9f4a5 in *__GI___strdup (s=0xbffff760 "/tmp/remake/remake") at strdup.c:42
;; #2  0x080593ac in main (argc=2, argv=0xbffff5a4, envp=0xbffff5b0)
;;    at main.c:952
;; #46 0xb7f51b87 in vm_call_cfunc (th=0x804d188, reg_cfp=0xb7ba9e88, num=0,
;;    recv=157798080, blockptr=0x0, me=0x80d12a0) at vm_insnhelper.c:410

(setf (gethash "debugger-backtrace" realgud:dlv-pat-hash)
      (make-realgud-loc-pat
       :regexp          (concat realgud:dlv-frame-start-regexp
                        realgud:dlv-frame-num-regexp
                        "\\(?:.\\|\\(?:[\n] \\)\\)+[ ]+at "
                        realgud:dlv-frame-file-regexp
                        )
       :num 1
       :file-group 2
       :line-group 3)
      )

(setf (gethash "font-lock-keywords" realgud:dlv-pat-hash)
      '(
        ;; #2  0x080593ac in main (argc=2, argv=0xbffff5a4, envp=0xbffff5b0)
        ;;    at main.c:952
        ("[ \n]+at \\(.*\\):\\([0-9]+\\)"
         (1 realgud-file-name-face)
         (2 realgud-line-number-face))

        ;; The frame number and first type name, if present.
        ;; E.g. =>#0  Makefile.in at /tmp/Makefile:216
        ;;      ---^
        ( "\\(?:^\\|\n\\)\\([0-9]+\\)  "
         (1 realgud-backtrace-number-face))
        ))

;;  Prefix used in variable names (e.g. short-key-mode-map) for
;; this debugger
(setf (gethash "dlv" realgud:variable-basename-hash) "realgud:dlv")

(defvar realgud:dlv-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'continue' and the value is
  the dlv command to use, like 'process continue'")

(setf (gethash "break"     realgud:dlv-command-hash) "b %X:%l")
(setf (gethash "backtrace" realgud:dlv-command-hash) "bt")
(setf (gethash "delete"    realgud:dlv-command-hash) "clear %p")
(setf (gethash "clear"     realgud:dlv-command-hash) "clear %p")
(setf (gethash "continue"  realgud:dlv-command-hash) "continue")
(setf (gethash "eval"      realgud:dlv-command-hash) "print %s")
(setf (gethash "finish"    realgud:dlv-command-hash) "stepout")
(setf (gethash "quit"      realgud:dlv-command-hash) "quit")
(setf (gethash "run"       realgud:dlv-command-hash) "run")
(setf (gethash "step"      realgud:dlv-command-hash) "step")
(setf (gethash "dlv" realgud-command-hash) realgud:dlv-command-hash)

(setf (gethash "dlv" realgud-pat-hash) realgud:dlv-pat-hash)

(provide-me "realgud:dlv-")
