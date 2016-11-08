;;==============================================================================
;;     File: ex1_32.scm
;;  Created: 11/04/2016, 14:09
;;   Author: Bernie Roesler
;;
;;  Description: Accumulate (general sum + product)
;;
;;==============================================================================

; (a) Linear recursive
; combiner   == procedure by which to combine terms
; null-value == value to use when we've run out of terms
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) 
              (accumulate combiner null-value term (next a) next b))))

; (b) Linear iterative
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))

; Write sum and product as simple calls to accumulate
(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

; sum integers
(define (sum-int a b)
  (define (identity a) a)
  (sum identity a 1+ b))

; factorial
(define (fact n)
  (define (identity x) x)
  (product identity 1 1+ n))

; Test code:
; (sum-int 1 10) ;Value: 55
; (fact 5)       ;Value: 120
;;==============================================================================
;;==============================================================================
