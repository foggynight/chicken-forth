;; chicken-forth.scm - FORTH implemented in CHICKEN Scheme.
;; Copyright (C) 2022 Robert Coffey
;; Released under the MIT license.

(import (chicken format)
        (chicken io)
        (chicken process-context)
        (chicken string)
        (srfi 25)
        (srfi 69))

;;> config <;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define DATA-STACK-SIZE (expt 2 16))

;;> misc <;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (1+ n) (+ n 1))
(define (1- n) (- n 1))

;;> array <;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (array-shift-left! arr start end)
  (do ((i start (+ i 1)))
      ((= i (1- end)))
    (array-set! arr i (array-ref arr (1+ i)))))

(define (array-shift-right! arr start end)
  (do ((i (1- end) (- i 1)))
      ((= i start))
    (array-set! arr i (array-ref arr (1- i)))))

;;> stack <;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type stack
  (%make-stack data size index)
  stack?
  (data stack-data)
  (size stack-size)
  (index stack-index stack-index-set!))

(define (make-stack size)
  (%make-stack (make-array (shape 0 size) 0) size 0))

(define (stack-index-inc! stk) (stack-index-set! stk (1+ (stack-index stk))))
(define (stack-index-dec! stk) (stack-index-set! stk (1- (stack-index stk))))

(define (stack-push! stk elem)
  (array-set! (stack-data stk) (stack-index stk) elem)
  (stack-index-inc! stk))
(define (stack-pop! stk) (stack-index-dec! stk))
(define (stack-top stk) (array-ref (stack-data stk) (1- (stack-index stk))))

(define (stack-shift! stk offset dir)
  (case dir
    ((left)
     (array-shift-left! (stack-data stk)
                        (- (stack-index stk) offset 1)
                        (stack-index stk))
     (stack-index-dec! stk))
    ((right)
     (array-shift-right! (stack-data stk)
                         (- (stack-index stk) offset)
                         (1+ (stack-index stk)))
     (stack-index-inc! stk))))

(define (stack-ref stk offset)
  (array-ref (stack-data stk) (- (stack-index stk) offset 1)))
(define (stack-set! stk offset elem)
  (array-set! (stack-data stk) (- (stack-index stk) offset 1) elem))

(define (stack-ins! stk offset elem)
  (stack-shift! stk offset 'right)
  (stack-set! stk offset elem))
(define (stack-drop! stk offset) (stack-shift! stk offset 'left))

;;> dictionary <;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-dict) (make-hash-table #:hash string-ci-hash #:test string-ci=?))
(define (dict-add! dict word code) (hash-table-set! dict word code))
(define (dict-ref dict word) (hash-table-ref dict word))
(define (dict-has? dict word) (hash-table-exists? dict word))

(define dict (make-dict))
(define-constant default-words
  '(drop dup over rot swap + - * /))
(for-each (lambda (e) (dict-add! dict (symbol->string e) e))
          default-words)

;;> code <;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> compiler <;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> executer <;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (exec-code! code stk)
  (case code
    ((drop) (stack-pop! stk))
    ((dup) (stack-push! stk (stack-top stk)))
    ((over) (stack-push! stk (stack-ref stk 1)))
    ((rot)
     (stack-push! stk (stack-ref stk 2))
     (stack-drop! stk 3))
    ((swap)
     (let ((t1 (stack-top stk)))
       (stack-pop! stk)
       (stack-ins! stk 1 t1)))
    ((+ - * /)
     (let ((a2 (stack-top stk)))
       (stack-pop! stk)
       (let ((a1 (stack-top stk)))
         (stack-pop! stk)
         (stack-push! stk ((eval code) a1 a2)))))
    ))

;;> runner <;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (number-valid? str)
  (let ((num (string->number str)))
    (and num (fixnum? num))))

(define (number-run! str stk)
  (stack-push! stk (string->number str)))

;;> interpreter <;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (inter-word! word stk)
  (cond ((dict-has? dict word) (exec-code! (dict-ref dict word) stk))
        ((number-valid? word) (number-run! word stk))
        (else (printf "undefined word: ~A~%" word))))

;;> main <;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-stack stk)
  (display "( ")
  (do ((i 0 (+ i 1)))
      ((= i (stack-index stk)))
    (display (array-ref (stack-data stk) i))
    (display #\space))
  (display ")")
  (newline))

(define (main)
  (define stk (make-stack DATA-STACK-SIZE))
  (do ((line (read-line) (read-line)))
      ((eof-object? line))
    (let ((words (string-split line)))
      (do ((lst words (cdr lst)))
          ((null? lst))
        (inter-word! (car lst) stk)
        (print-stack stk)))))

(let ((args (command-line-arguments)))
  (cond ((null? args) (main))
        (else (with-input-from-file (car args) main))))
