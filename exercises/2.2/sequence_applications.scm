;;==============================================================================
;;     File: sequence_applications.scm
;;  Created: 11/12/2016, 17:51
;;   Author: Bernie Roesler
;;
;;  Description: Sequence operations examples 
;;
;;==============================================================================

;;; Recursive definition
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)  
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define x (list 1 (list 2 (list 3 4)) 5))
(printval (sum-odd-squares x))  ; Value: 35 (== 1 + 9 + 25)

;;; Even Fibonacci numbers list (recursive)
(load "../../ch1/1.2/fibonacci.scm") ; load (fib n) procedure
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(printval (even-fibs 10)) ; Value: (0 2 8 34)

;------------------------------------------------------------------------------- 
;       Add sequence operations
;-------------------------------------------------------------------------------
(load "sequence_operations.scm")

;;; Redefine as a signal flow
(define (sum-odd-squares tree)
  (accumulate + 
              0 
              (map square 
                   (filter odd? 
                           (enumerate-tree tree)))))

(printval (sum-odd-squares x))  ; Value: 35 (== 1 + 9 + 25)

;;; Redefine even fibs
(define (even-fibs n)
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(printval (even-fibs 10)) ; Value: (0 2 8 34)
;;==============================================================================
;;==============================================================================
