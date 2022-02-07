;; chicken-forth.scm - FORTH implemented in CHICKEN Scheme.
;; Copyright (C) 2022 Robert Coffey
;; Released under the MIT license.

(import (chicken io)
        (chicken process-context))

(define dictionary)

(define (repl)
  (let ((line (read-line)))
    (when (eof-object? line)
      (exit))
    (print "ok"))
  (repl))

(let ((args (command-line-arguments)))
  (cond ((null? args) (repl))
        (else (with-input-from-file (car args) repl))))
