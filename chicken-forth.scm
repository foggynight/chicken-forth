;; chicken-forth.scm - FORTH-like implemented in CHICKEN Scheme.
;; Copyright (C) 2022 Robert Coffey
;; Released under the MIT license.

(import (chicken bitwise)
        (chicken format)
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

(: stack-drop! (stack fixnum -> void))
(define (stack-drop! stk offset)
  (stack-shift! stk offset 'left))

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

(define-constant STATE-INTERPRET #f)
(define-constant STATE-COMPILE   #t)

(define-constant VERSION "0.1.0")

(: state boolean) (define state STATE-INTERPRET)
(: base fixnum) (define base 10)

(: pstk stack) (define pstk (make-stack (expt 2 16)))
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
  (stack-push! pstk (stack-top pstk)))

(def-code "over" 0
  (stack-push! pstk (stack-ref pstk 1)))

(def-code "drop" 0
  (stack-pop! pstk))

(def-code "nip" 0
  (let ((t (stack-pop! pstk)))
    (stack-set! pstk 0 t)))

(def-code "swap" 0
  (let ((t (stack-pop! pstk)))
    (stack-ins! pstk 1 t)))

(def-code "rot" 0
  (stack-push! pstk (stack-ref pstk 2))
  (stack-drop! pstk 3))

;; arithmetic primitives

(def-code "+" 0
  (stack-push! pstk (+ (stack-pop! pstk) (stack-pop! pstk))))

(def-code "-" 0
  (let ((t (stack-pop! pstk)))
    (stack-push! pstk (- (stack-pop! pstk) t))))

(def-code "*" 0
  (stack-push! pstk (* (stack-pop! pstk) (stack-pop! pstk))))

(def-code "/" 0
  (let ((t (stack-pop! pstk)))
    (stack-push! pstk (// (stack-pop! pstk) t))))

(def-code "2+" 0
  (stack-push! pstk (+ (stack-pop! pstk) 2)))

(def-code "2-" 0
  (stack-push! pstk (- (stack-pop! pstk) 2)))

(def-code "4+" 0
  (stack-push! pstk (+ (stack-pop! pstk) 4)))

(def-code "4-" 0
  (stack-push! pstk (- (stack-pop! pstk) 4)))

;; comparison primitives

(def-code "=" 0
  (stack-push! pstk (if (= (stack-pop! pstk) (stack-pop! pstk)) TRUE FALSE)))

(def-code "<>" 0
  (stack-push! pstk (if (= (stack-pop! pstk) (stack-pop! pstk)) FALSE TRUE)))

(def-code "<" 0
  (let ((t (stack-pop! pstk)))
    (stack-push! pstk (if (< (stack-pop! pstk) t) TRUE FALSE))))

(def-code ">" 0
  (let ((t (stack-pop! pstk)))
    (stack-push! pstk (if (> (stack-pop! pstk) t) TRUE FALSE))))

(def-code "<=" 0
  (let ((t (stack-pop! pstk)))
    (stack-push! pstk (if (<= (stack-pop! pstk) t) TRUE FALSE))))

(def-code ">=" 0
  (let ((t (stack-pop! pstk)))
    (stack-push! pstk (if (>= (stack-pop! pstk) t) TRUE FALSE))))

(def-code "0=" 0
  (stack-push! pstk (if (zero? (stack-pop! pstk)) TRUE FALSE)))

(def-code "0<>" 0
  (stack-push! pstk (if (zero? (stack-pop! pstk)) FALSE TRUE)))

(def-code "0<" 0
  (let ((t (stack-pop! pstk)))
    (stack-push! pstk (if (negative? (stack-pop! pstk)) TRUE FALSE))))

(def-code "0>" 0
  (let ((t (stack-pop! pstk)))
    (stack-push! pstk (if (positive? (stack-pop! pstk)) TRUE FALSE))))

(def-code "0<=" 0
  (let ((t (stack-pop! pstk)))
    (stack-push! pstk (if (<= (stack-pop! pstk) 0) TRUE FALSE))))

(def-code "0>=" 0
  (let ((t (stack-pop! pstk)))
    (stack-push! pstk (if (>= (stack-pop! pstk) 0) TRUE FALSE))))

;; bitwise primitives

(def-code "and" 0
  (stack-push! pstk (bitwise-and (stack-pop! pstk) (stack-pop! pstk))))

(def-code "or" 0
  (stack-push! pstk (bitwise-ior (stack-pop! pstk) (stack-pop! pstk))))

(def-code "xor" 0
  (stack-push! pstk (bitwise-xor (stack-pop! pstk) (stack-pop! pstk))))

(def-code "invert" 0
  (stack-push! pstk (bitwise-not (stack-pop! pstk))))

;; input and output

(def-code "." 0
  (printf "~A " (stack-pop! pstk)))

(def-code "emit" 0
  (printf "~A" (integer->char (stack-pop! pstk))))

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
    (display (array-ref (stack-data stk) i))
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
  (do ((word (parse-word) (parse-word)))
      ((or (not word) (eof-object? word)))
    (let ((entry (dict-ref dict word)))
      (cond ((not (not entry)) ((entry-code entry)))
            ((number-valid? word) (number-run! word pstk))
            (else (printf "undefined word: ~A~%" word))))))

(let ((args (command-line-arguments)))
  (cond ((null? args) (main))
        (else (with-input-from-file (car args) main))))
