;; chicken-forth.scm - FORTH-like implemented in CHICKEN Scheme.
;; Copyright (C) 2022 Robert Coffey
;; Released under the MIT license.

(import (chicken bitwise)
        (chicken format)
        (chicken io)
        (chicken process-context)
        (srfi 25)
        procedural-macros)

;;; utility ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: 1+ (number --> number)) (define (1+ n) (+ n 1))
(: 1- (number --> number)) (define (1- n) (- n 1))

(: // (number number --> fixnum))
(define (// n d) (inexact->exact (floor (/ n d))))

(define-type array (struct array))

(: array-shift-left! (array fixnum fixnum -> void))
(define (array-shift-left! arr start end)
  (do ((i start (1+ i)))
      ((= i (1- end)))
    (array-set! arr i (array-ref arr (1+ i)))))

(: array-shift-right! (array fixnum fixnum -> void))
(define (array-shift-right! arr start end)
  (do ((i (1- end) (1- i)))
      ((= i start))
    (array-set! arr i (array-ref arr (1- i)))))

;;; stack ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type stack (struct stack))

(define-record-type stack
  (%make-stack data size index)
  stack?
  (data stack-data)
  (size stack-size)
  (index stack-index stack-index-set!))

(: make-stack (fixnum --> stack))
(define (make-stack size)
  (%make-stack (make-array (shape 0 size) 0) size 0))

(: stack-index-inc! (stack -> void))
(define (stack-index-inc! stk)
  (stack-index-set! stk (1+ (stack-index stk))))

(: stack-index-dec! (stack -> void))
(define (stack-index-dec! stk)
  (stack-index-set! stk (1- (stack-index stk))))

(: stack-push! (stack any -> void))
(define (stack-push! stk elem)
  (array-set! (stack-data stk) (stack-index stk) elem)
  (stack-index-inc! stk))

(: stack-top (stack --> any))
(define (stack-top stk)
  (array-ref (stack-data stk) (1- (stack-index stk))))

(: stack-pop! (stack -> any))
(define (stack-pop! stk)
  (let ((t (stack-top stk))) (stack-index-dec! stk) t))

(: stack-shift! (stack fixnum symbol -> void))
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

(: stack-ref (stack fixnum --> any))
(define (stack-ref stk offset)
  (array-ref (stack-data stk) (- (stack-index stk) offset 1)))

(: stack-set! (stack fixnum any -> void))
(define (stack-set! stk offset elem)
  (array-set! (stack-data stk) (- (stack-index stk) offset 1) elem))

(: stack-ins! (stack fixnum any -> void))
(define (stack-ins! stk offset elem)
  (stack-shift! stk offset 'right)
  (stack-set! stk offset elem))

(: stack-drop! (stack fixnum #!optional fixnum -> void))
(define (stack-drop! stk offset #!optional (count 1))
  (do ((i 0 (1+ i)))
      ((>= i count))
    (stack-shift! stk offset 'left)))

;;; dictionary ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The dictionary contains the wordlist of the program, it is represented by a
;; list of entry records.

(define-type entry (struct entry))
(define-type dict (list-of entry))

(define-record-type entry
  (make-entry name flag code)
  entry?
  (name entry-name entry-name-set!)
  (flag entry-flag entry-flag-set!)
  (code entry-code entry-code-set!))

(: dict-ref (dict string --> (or entry false)))
(define (dict-ref dict word)
  (let loop ((lst dict))
    (cond ((null? lst) #f)
          ((string-ci=? word (entry-name (car lst))) (car lst))
          (else (loop (cdr lst))))))

(: dict-has? (dict string --> boolean))
(define (dict-has? dict word)
  (not (not (dict-ref dict word))))

;;; global ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant TRUE -1)
(define-constant FALSE 0)

(define-constant STATE-IMD #f) ; immediate mode
(define-constant STATE-CMP #t) ; compile mode

(define-constant VERSION "0.1.0")

(: state boolean) (define state STATE-IMD)
(: radix fixnum) (define radix 10)

(: lstk stack) (define lstk (make-stack (expt 2 16)))
(: rstk stack) (define rstk (make-stack (expt 2 16)))
(: dict dict) (define dict '())

;;; primitives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add an entry to the dictionary with the members NAME and FLAG, CODE is
;; created by wrapping the given expression body in REST in a lambda. This macro
;; also defines a procedure named forth-NAME which points to that lambda.
(define-macro (def-code name flag . rest)
  (let ((thunk `(lambda () ,@rest)))
    `(begin (set! dict (cons (make-entry ,name ,flag ,thunk) dict))
            (define (,(string->symbol (string-append "forth-" name)))
              (,thunk)))))

;; stack manipulation primitives

(def-code "dup" 0
  (stack-push! lstk (stack-top lstk)))

(def-code "?dup" 0
  (unless (zero? (stack-top lstk))
    (stack-push! lstk (stack-top lstk))))

(def-code "over" 0
  (stack-push! lstk (stack-ref lstk 1)))

(def-code "drop" 0
  (stack-pop! lstk))

(def-code "nip" 0
  (let ((t (stack-pop! lstk)))
    (stack-set! lstk 0 t)))

(def-code "swap" 0
  (let ((t (stack-pop! lstk)))
    (stack-ins! lstk 1 t)))

(def-code "rot" 0
  (stack-push! lstk (stack-ref lstk 2))
  (stack-drop! lstk 3))

(def-code "-rot" 0
  (stack-ins! lstk 3 (stack-top lstk))
  (stack-pop! lstk))

(def-code "2dup" 0
  (stack-push! lstk (stack-ref lstk 1))
  (stack-push! lstk (stack-ref lstk 1)))

(def-code "2drop" 0
  (stack-drop! lstk 0 2))

(def-code "2swap" 0
  (let ((t1 (stack-pop! lstk))
        (t2 (stack-pop! lstk)))
    (stack-ins! lstk 2 t1)
    (stack-ins! lstk 2 t2)))

(def-code ">r" 0
  (stack-push! rstk (stack-pop! lstk)))

(def-code "r>" 0
  (stack-push! lstk (stack-pop! rstk)))

(def-code "rdrop" 0
  (stack-pop! rstk))

;; arithmetic primitives

(def-code "+" 0
  (stack-push! lstk (+ (stack-pop! lstk) (stack-pop! lstk))))

(def-code "-" 0
  (let ((t (stack-pop! lstk)))
    (stack-push! lstk (- (stack-pop! lstk) t))))

(def-code "*" 0
  (stack-push! lstk (* (stack-pop! lstk) (stack-pop! lstk))))

(def-code "/" 0
  (let ((t (stack-pop! lstk)))
    (stack-push! lstk (// (stack-pop! lstk) t))))

(def-code "2+" 0
  (stack-push! lstk (+ (stack-pop! lstk) 2)))

(def-code "2-" 0
  (stack-push! lstk (- (stack-pop! lstk) 2)))

(def-code "4+" 0
  (stack-push! lstk (+ (stack-pop! lstk) 4)))

(def-code "4-" 0
  (stack-push! lstk (- (stack-pop! lstk) 4)))

(def-code "mod" 0
  (let ((t (stack-pop! lstk)))
    (stack-push! lstk (modulo (stack-pop! lstk) t))))

(def-code "/mod" 0
  (let ((t1 (stack-pop! lstk))
        (t2 (stack-pop! lstk)))
    (stack-push! lstk (// t2 t1))
    (stack-push! lstk (modulo t2 t1))))

(def-code "mod/" 0
  (let ((t1 (stack-pop! lstk))
        (t2 (stack-pop! lstk)))
    (stack-push! lstk (modulo t2 t1))
    (stack-push! lstk (// t2 t1))))

;; comparison primitives

(def-code "=" 0
  (stack-push! lstk (if (= (stack-pop! lstk) (stack-pop! lstk)) TRUE FALSE)))

(def-code "<>" 0
  (stack-push! lstk (if (= (stack-pop! lstk) (stack-pop! lstk)) FALSE TRUE)))

(def-code "<" 0
  (let ((t (stack-pop! lstk)))
    (stack-push! lstk (if (< (stack-pop! lstk) t) TRUE FALSE))))

(def-code ">" 0
  (let ((t (stack-pop! lstk)))
    (stack-push! lstk (if (> (stack-pop! lstk) t) TRUE FALSE))))

(def-code "<=" 0
  (let ((t (stack-pop! lstk)))
    (stack-push! lstk (if (<= (stack-pop! lstk) t) TRUE FALSE))))

(def-code ">=" 0
  (let ((t (stack-pop! lstk)))
    (stack-push! lstk (if (>= (stack-pop! lstk) t) TRUE FALSE))))

(def-code "0=" 0
  (stack-push! lstk (if (zero? (stack-pop! lstk)) TRUE FALSE)))

(def-code "0<>" 0
  (stack-push! lstk (if (zero? (stack-pop! lstk)) FALSE TRUE)))

(def-code "0<" 0
  (let ((t (stack-pop! lstk)))
    (stack-push! lstk (if (negative? (stack-pop! lstk)) TRUE FALSE))))

(def-code "0>" 0
  (let ((t (stack-pop! lstk)))
    (stack-push! lstk (if (positive? (stack-pop! lstk)) TRUE FALSE))))

(def-code "0<=" 0
  (let ((t (stack-pop! lstk)))
    (stack-push! lstk (if (<= (stack-pop! lstk) 0) TRUE FALSE))))

(def-code "0>=" 0
  (let ((t (stack-pop! lstk)))
    (stack-push! lstk (if (>= (stack-pop! lstk) 0) TRUE FALSE))))

;; bitwise primitives

(def-code "and" 0
  (stack-push! lstk (bitwise-and (stack-pop! lstk) (stack-pop! lstk))))

(def-code "or" 0
  (stack-push! lstk (bitwise-ior (stack-pop! lstk) (stack-pop! lstk))))

(def-code "xor" 0
  (stack-push! lstk (bitwise-xor (stack-pop! lstk) (stack-pop! lstk))))

(def-code "invert" 0
  (stack-push! lstk (bitwise-not (stack-pop! lstk))))

;; input and output

(def-code "key" 0
  (define byte (read-byte))
  (if (eof-object? byte)
      (exit)
      (stack-push! lstk byte)))

(def-code "word" 0
  (define (next-char)
    (forth-key)
    (integer->char (stack-pop! lstk)))
  (define str "")
  (let loop ((skip #f)
             (c (next-char)))
    (if skip
        (if (char-whitespace? c)
            (loop #t (next-char))
            (begin (set! str (string-append str (string c)))
                   (loop #f (next-char))))
        (unless (char-whitespace? c)
          (set! str (string-append str (string c)))
          (loop #f (next-char)))))
  (stack-push! lstk str)
  (stack-push! lstk (string-length str)))

(def-code "number" 0
  (define len (stack-pop! lstk))
  (define str (stack-pop! lstk))
  (let ((n (string->number str radix)))
    (stack-push! lstk (if n n 0))
    (stack-push! lstk (if n 0 -1))))

(def-code "emit" 0
  (printf "~A" (integer->char (stack-pop! lstk))))

(def-code "." 0
  (printf "~A " (stack-pop! lstk)))

;;; runner ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: number-valid? (string --> boolean))
(define (number-valid? str)
  (let ((num (string->number str)))
    (and num (fixnum? num))))

(: number-run! (string stack -> void))
(define (number-run! str stk)
  (stack-push! stk (string->number str)))

;;; main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: print-stack (stack -> void))
(define (print-stack stk)
  (display "( ")
  (do ((i 0 (1+ i)))
      ((= i (stack-index stk)))
    (write (array-ref (stack-data stk) i))
    (display #\space))
  (display ") ")
  (flush-output))

(: parse-word (-> (or string false)))
(define (parse-word)
  (define word "")
  (let loop ((read #f)
             (char (read-char)))
    (if read
        (if (or (eof-object? char) (char-whitespace? char))
            word
            (begin (set! word (string-append word (string char)))
                   (loop #t (read-char))))
        (cond ((eof-object? char) #f)
              ((char-whitespace? char) (loop #f (read-char)))
              (else (loop #t char))))))

(: main (-> void))
(define (main)
  (print-stack lstk)
  (do ((word (parse-word) (parse-word)))
      ((or (not word) (eof-object? word)))
    (let ((entry (dict-ref dict word)))
      (cond ((not (not entry)) ((entry-code entry)))
            ((number-valid? word) (number-run! word lstk))
            (else (printf "undefined word: ~A~%" word))))
    (print-stack lstk)))

(let ((args (command-line-arguments)))
  (cond ((null? args) (main))
        (else (with-input-from-file (car args) main))))
