;; chicken-forth.scm - FORTH-like implemented in CHICKEN Scheme
;; Copyright (C) 2022 Robert Coffey
;; Released under the MIT license.

(import (chicken bitwise)
        (chicken format)
        (chicken io)
        (chicken process-context)
        (srfi 25)
        procedural-macros
        vector-lib)

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

(define-record-type stack
  (%make-stack data size index)
  stack?
  (data stack-data)                     ; data buffer represented by array
  (size stack-size)                     ; size of data buffer in cells
  (index stack-index stack-index-set!)) ; index of next free cell in data buffer

(define-type stack (struct stack))

(define-record-printer (stack rec op)
  (fprintf op "<~A>" (stack-index rec))
  (do ((i 0 (1+ i)))
      ((= i (stack-index rec)))
    (fprintf op " ~A" (array-ref (stack-data rec) i))))

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

(define-constant FLAG-HIDDEN    #x1)
(define-constant FLAG-IMMEDIATE #x2)

(define-record-type entry
  (make-entry name flag code)
  entry?
  (name entry-name entry-name-set!)
  (flag entry-flag entry-flag-set!)
  (code entry-code entry-code-set!))

(define-type entry (struct entry))
(define-type dict (list-of entry))

(: entry-hidden? (entry --> boolean))
(define (entry-hidden? entry)
  (not (zero? (bitwise-and (entry-flag entry) FLAG-HIDDEN))))

(: entry-immediate? (entry --> boolean))
(define (entry-immediate? entry)
  (not (zero? (bitwise-and (entry-flag entry) FLAG-IMMEDIATE))))

(: entry-hidden-toggle! (entry -> void))
(define (entry-hidden-toggle! entry)
  (entry-flag-set! entry (bitwise-xor (entry-flag entry) FLAG-HIDDEN)))

(: entry-immediate-toggle! (entry -> void))
(define (entry-immediate-toggle! entry)
  (entry-flag-set! entry (bitwise-xor (entry-flag entry) FLAG-IMMEDIATE)))

(: dict-ref (dict string --> (or entry false)))
(define (dict-ref dict word)
  (let loop ((lst dict))
    (cond ((null? lst) #f)
          ((and (not (entry-hidden? (car lst)))
                (string-ci=? word (entry-name (car lst))))
           (car lst))
          (else (loop (cdr lst))))))

(: dict-has? (dict string --> boolean))
(define (dict-has? dict word)
  (not (not (dict-ref dict word))))

;;; global ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: VERSION string) (define-constant VERSION "0.1.0")

(: TRUE fixnum)  (define-constant TRUE -1)
(: FALSE fixnum) (define-constant FALSE 0)

(: STATE-EXECUTE boolean) (define-constant STATE-EXECUTE #f)
(: STATE-COMPILE boolean) (define-constant STATE-COMPILE #t)

(: lstk stack) (define lstk (make-stack (expt 2 16)))
(: rstk stack) (define rstk (make-stack (expt 2 16)))
(: dict dict)  (define dict '())

(: next vector)   (define next  #())
(: radix fixnum)  (define radix 10)
(: state boolean) (define state STATE-EXECUTE)

;;; docol ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: docol (any -> fixnum))
(define (docol elem)
  (cond ((procedure? elem) (elem) -1)
        ((vector? elem)
         (do ((i 0 (1+ i)))
             ((= i (vector-length elem)))
           (let ((ii (docol (vector-ref elem i))))
             (unless (= ii -1) (set! i (1- ii))))))
        (else (stack-push! lstk elem) -1)))

;;; forth ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add an entry to the dictionary with the members NAME and FLAG, CODE is
;; created by wrapping the given expression body in REST in a lambda. This macro
;; also defines a variable named forth-NAME which is bound to that lambda.
(define-macro (def-code name flag . rest)
  (let ((thunk `(lambda () ,@rest)))
    `(begin (set! dict (cons (make-entry ,name ,flag ,thunk) dict))
            (define ,(string->symbol (string-append "forth-" name)) ,thunk))))

;; Add an entry to the dictionary using DEF-CODE with the member NAME and a null
;; FLAG, CODE is a procedure which pushes VALUE on the data stack.
(define-syntax def-const
  (syntax-rules ()
    ((_ name value)
     (def-code name 0
       (stack-push! lstk value)))))

;; forth constants

(def-const "version" VERSION)

;; stack manipulation

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
    (stack-ins! lstk 2 t2)
    (stack-ins! lstk 2 t1)))

(def-code ">r" 0
  (stack-push! rstk (stack-pop! lstk)))

(def-code "r>" 0
  (stack-push! lstk (stack-pop! rstk)))

(def-code "rdrop" 0
  (stack-pop! rstk))

;; arithmetic operators

(def-code "+" 0
  (stack-push! lstk (+ (stack-pop! lstk) (stack-pop! lstk))))

(def-code "-" 0
  (let ((t (stack-pop! lstk)))
    (stack-push! lstk (- (stack-pop! lstk) t))))

(def-code "*" 0
  (stack-push! lstk (* (stack-pop! lstk) (stack-pop! lstk))))

(def-code "/" 0
  (let ((t (stack-pop! lstk)))
    (stack-push! lstk (/ (stack-pop! lstk) t))))

(def-code "//" 0
  (let ((t (stack-pop! lstk)))
    (stack-push! lstk (// (stack-pop! lstk) t))))

(def-code "1+" 0
  (stack-push! lstk (1+ (stack-pop! lstk))))

(def-code "1-" 0
  (stack-push! lstk (1- (stack-pop! lstk))))

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

;; comparison operators

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

;; bitwise operators

(def-code "not" 0
  (stack-push! lstk (bitwise-not (stack-pop! lstk))))

(def-code "and" 0
  (stack-push! lstk (bitwise-and (stack-pop! lstk) (stack-pop! lstk))))

(def-code "or" 0
  (stack-push! lstk (bitwise-ior (stack-pop! lstk) (stack-pop! lstk))))

(def-code "xor" 0
  (stack-push! lstk (bitwise-xor (stack-pop! lstk) (stack-pop! lstk))))

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
  (let loop ((skip #t)
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
  (display (integer->char (stack-pop! lstk))))

(def-code "." 0
  (printf "~A " (stack-pop! lstk)))

(def-code ".s" 0
  (print lstk))

(def-code "write" 0
  (write (stack-pop! lstk)))

(def-code "display" 0
  (display (stack-pop! lstk)))

(def-code "print" 0
  (print (stack-pop! lstk)))

;; dictionary and compiler

(def-code "create" 0
  (define len (stack-pop! lstk))
  (define str (stack-pop! lstk))
  (define entry (make-entry str 0 (void)))
  (set! dict (cons entry dict)))

(def-code "latest" 0
  (stack-push! lstk (car dict)))

(def-code "hidden" 0
  (define entry (stack-pop! lstk))
  (entry-hidden-toggle! entry))

(def-code "immediate" 0
  (define entry (stack-pop! lstk))
  (entry-immediate-toggle! entry))

(def-code "find" 0
  (define len (stack-pop! lstk))
  (define str (stack-pop! lstk))
  (let ((entry (dict-ref dict str)))
    (stack-push! lstk (if entry entry FALSE))))

(def-code "code" 0
  (define entry (stack-pop! lstk))
  (stack-push! lstk (entry-code entry)))

(def-code "," 0
  (set! next (vector-append next (vector (stack-pop! lstk)))))

(def-code "[" FLAG-IMMEDIATE
  (set! state STATE-EXECUTE))

(def-code "]" 0
  (set! state STATE-COMPILE))

(def-code ":" 0
  (forth-word)
  (forth-create)
  (forth-latest)
  (forth-hidden)
  (|forth-]|))

(def-code ";" FLAG-IMMEDIATE
  (forth-latest)
  (entry-code-set! (stack-pop! lstk) next)
  (set! next #())
  (forth-latest)
  (forth-hidden)
  (|forth-[|))

;; interpreter

(def-code "interpret" 0
  (forth-word)
  (define length (stack-ref lstk 0))
  (define input (stack-ref lstk 1))
  (forth-find)
  (let ((entry (stack-pop! lstk)))
    (if (eqv? entry FALSE)
        ;; assume input was a number
        (begin (stack-push! lstk input)
               (stack-push! lstk length)
               (forth-number)
               (if (zero? (stack-pop! lstk))
                   (when (eqv? state STATE-COMPILE)
                     (|forth-,|))
                   (begin (stack-pop! lstk)
                          (printf "unknown word: ~A~%" input))))
        ;; input is a word in the dictionary
        (let ((se (eqv? state STATE-EXECUTE))
              (ei (entry-immediate? entry)))
          (if (or se ei)
              (if (and se ei)
                  (printf "interpreted compile-only word: ~A~%" input)
                  (docol (entry-code entry)))
              (begin (stack-push! lstk (entry-code entry))
                     (|forth-,|)))))))

(def-code "quit" 0
  (forth-interpret)
  (forth-quit))

;;; main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: main (-> void))
(define (main)
  (forth-quit))

(let ((args (command-line-arguments)))
  (cond ((null? args) (main))
        (else (with-input-from-file (car args) main))))
