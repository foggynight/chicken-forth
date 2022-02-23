;; chicken-forth.scm - FORTH-like implemented in CHICKEN Scheme.
;; Copyright (C) 2022 Robert Coffey
;; Released under the MIT license.

(import (chicken format)
        (chicken process-context)
        (srfi 25)
        procedural-macros)

;; config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define DATA-STACK-SIZE (expt 2 16))

;; misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (1+ n) (+ n 1))
(define (1- n) (- n 1))

;; array ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (array-shift-left! arr start end)
  (do ((i start (+ i 1)))
      ((= i (1- end)))
    (array-set! arr i (array-ref arr (1+ i)))))

(define (array-shift-right! arr start end)
  (do ((i (1- end) (- i 1)))
      ((= i start))
    (array-set! arr i (array-ref arr (1- i)))))

;; stack ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; dictionary ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The dictionary contains the wordlist of the program. Each word defined in a
;; program contains an entry in the dictionary, this entry defines its name and
;; how to execute it.
;;
;; The dictionary is represented by a record containing a list of dotted pairs,
;; where the keys are the names of the words converted from strings into
;; symbols, and the datums are one of the following:
;;
;; - symbol: A primitive word, these have a clause in the EXEC-PRIM! procedure.
;; - list: A list containing zero or more words and/or numbers.

(define-record-type dict
  (%make-dict lst)
  dict?
  (lst dict-lst dict-lst-set!))

(define (make-dict . rest) (%make-dict rest))

(define (dict-add! dict word code)
  (dict-lst-set! dict (cons (cons word code) (dict-lst dict))))

(define (dict-ref dict word)
  (let ((code (assq word (dict-lst dict))))
    (if code (cdr code) #f)))

(define (dict-has? dict word)
  (not (not (dict-ref dict word))))

(define-constant primitives
  '(: drop dup over rot swap + - * / , read |.| write lf cr newline))

(define (default-dict)
  (define dict (make-dict))
  (for-each (lambda (e) (dict-add! dict e e))
            primitives)
  dict)

;; parser ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; runner ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (number-valid? str)
  (let ((num (string->number str)))
    (and num (fixnum? num))))

(define (number-run! str stk)
  (stack-push! stk (string->number str)))

;; compiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (comp-code! dict)
  (define name (parse-word))
  (define code
    (let loop ((word (parse-word)))
      (if (string=? word ";")
          '()
          (let ((sym (string->symbol word)))
            (cond ((dict-has? dict sym) (cons sym (loop (parse-word))))
                  ((number-valid? word) (cons (string->number word)
                                              (loop (parse-word))))
                  (else #f))))))
  (if (not code)
      (printf "failed to compile: ~A~%" name)
      (dict-add! dict (string->symbol name) code)))

;; executer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (exec-prim! code stk dict)
  (case code
    ((:) (comp-code! dict))
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
    ((, read) (stack-push! stk (read-byte)))
    ((|.| write) (write (stack-top stk)) (stack-pop! stk))
    ((lf cr newline) (newline))
    ))

(define (exec-list! lst stk dict)
  (for-each (lambda (word)
              (cond ((number? word) (stack-push! stk word))
                    ((symbol? word)
                     (let ((code (dict-ref dict word)))
                       (if code
                           (exec-code! code stk dict)
                           (printf "undefined word: ~A~%" word))))
                    (else (printf "invalid word: ~A~%" word))))
            lst))

(define (exec-code! code stk dict)
  (if (symbol? code)
      (exec-prim! code stk dict)
      (exec-list! code stk dict)))

;; interpreter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (inter-word! word stk dict)
  (define code (dict-ref dict (string->symbol word)))
  (cond ((not (not code)) (exec-code! code stk dict))
        ((number-valid? word) (number-run! word stk))
        (else (printf "undefined word: ~A~%" word))))

;; main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-stack stk)
  (display "( ")
  (do ((i 0 (+ i 1)))
      ((= i (stack-index stk)))
    (display (array-ref (stack-data stk) i))
    (display #\space))
  (display ")")
  (newline))

(define (print-dict dict)
  (print (dict-lst dict)))

(define (main)
  (define stk (make-stack DATA-STACK-SIZE))
  (define dict (default-dict))
  (do ((word (parse-word) (parse-word)))
      ((or (not word) (eof-object? word)))
    (inter-word! word stk dict)))

(let ((args (command-line-arguments)))
  (cond ((null? args) (main))
        (else (with-input-from-file (car args) main))))
