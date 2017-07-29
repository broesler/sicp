;;==============================================================================
;;     File: ex3_27.scm
;;  Created: 01/17/2017, 18:10
;;   Author: Bernie Roesler
;;
;;  Description: Memoization of Fibonacci procedure
;;
;;==============================================================================

;; local tables
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
          (cdr record)
          false)))
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
          (set-cdr! record value)
          (set-cdr! local-table
                    (cons (cons key value)
                          (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'print) local-table)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

; (define table (make-table))
; (define get (table 'lookup))
; (define put (table 'insert!))

; (put 1 'a)
; (put 2 'b)
; (get 1)
; (get 2)
; (table 'print)

;;; Code in book:
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result ((table 'lookup) x)))
        (or previously-computed-result
            (let ((result (f x)))
              ((table 'insert!) x result)
              result))))))

(define memo-fib
  (memoize 
    (lambda (n)
      (cond ((= n 0) 0)
            ((= n 1) 1)
            (else (+ (memo-fib (- n 1))
                     (memo-fib (- n 2))))))))

(memo-fib 3) ; Value: 2

;;; (define memo-fib (memoize fib)) will NOT work because the recursive call to
;;; fib will just stay within the global environment and "forget" the table

;;==============================================================================
;;==============================================================================
