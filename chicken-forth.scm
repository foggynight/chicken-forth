;; chicken-forth.scm - FORTH implemented in CHICKEN Scheme.
;; Copyright (C) 2022 Robert Coffey
;; Released under the MIT license.

(import (chicken format)
        (chicken io)
        (chicken process-context)
        (chicken string)
        (srfi 1)
        (srfi 69)
        format)

;;> misc <;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (1+ n) (+ n 1))
(define (1- n) (- n 1))

;;> list <;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-drop lst i)
  (cond ((null? lst) '())
        ((positive? i) (cons (car lst) (list-drop (cdr lst) (1- i))))
        ((zero? i) (cdr lst))
        (else lst)))

(define (list-insert lst i elm)
  (cond ((null? lst) (cons elm '()))
        ((positive? i) (cons (car lst) (list-insert (cdr lst) (1- i) elm)))
        (else (cons elm lst))))

;;> stack <;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define stk '())
(define (stk-drop! i) (set! stk (list-drop stk i)))
(define (stk-ins! i elm) (set! stk (list-insert stk i elm)))
(define (stk-pop!) (set! stk (cdr stk)))
(define (stk-push! elm) (set! stk (cons elm stk)))
(define (stk-ref i) (list-ref stk i))
(define (stk-top) (car stk))

;;> dictionary <;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dict (make-hash-table #:hash string-ci-hash #:test string-ci=?))
(define (dict-add! word code) (hash-table-set! dict word code))
(define (dict-ref word) (hash-table-ref dict word))
(define (dict-has? word) (hash-table-exists? dict word))

(define-constant default-words
  '(drop dup over rot swap + - * /))
(for-each (lambda (e) (dict-add! (symbol->string e) e))
          default-words)

;;> compiler <;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> executer <;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (exec-code! code)
  (case code
    ((drop) (stk-pop!))
    ((dup) (stk-push! (stk-top)))
    ((over) (stk-push! (stk-ref 1)))
    ((rot)
     (stk-push! (stk-ref 2))
     (stk-drop! 3))
    ((swap)
     (let ((t1 (stk-top)))
       (stk-pop!)
       (stk-ins! 1 t1)))
    ((+ - * /)
     (let ((a2 (stk-top)))
       (stk-pop!)
       (let ((a1 (stk-top)))
         (stk-pop!)
         (stk-push! ((eval code) a1 a2)))))
    ))

;;> runner <;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (num-valid? str)
  (let ((num (string->number str)))
    (and num (fixnum? num))))

(define (num-run! str)
  (stk-push! (string->number str)))

;;> interpreter <;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (inter-word! word)
  (cond ((dict-has? word) (exec-code! (dict-ref word)))
        ((num-valid? word) (num-run! word))
        (else (printf "undefined word: ~A~%" word))))

;;> repl <;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-stk cpy stk)
  (format #t "( ~{~A ~}" (reverse cpy))
  (format #t "-- ~{~A ~})~%" (reverse stk)))

(define (repl)
  (let ((line (read-line)))
    (when (eof-object? line)
      (exit))
    (let ((words (string-split line)))
      (do ((lst words (cdr lst))
           (cpy (list-copy stk) (list-copy stk)))
          ((null? lst))
        (inter-word! (car lst))
        (print-stk cpy stk))))
  (repl))

(let ((args (command-line-arguments)))
  (cond ((null? args) (repl))
        (else (with-input-from-file (car args) repl))))
