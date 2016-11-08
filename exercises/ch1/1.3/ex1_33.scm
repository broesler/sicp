;;==============================================================================
;;     File: ex1_33.scm
;;  Created: 11/04/2016, 14:19
;;   Author: Bernie Roesler
;;
;;  Description: Even more general accumulate --> filter
;;
;;==============================================================================
; filtered-accumulate: only combine those terms that are derived from values in
;   the range that satisfy a specified condition
(define (filtered-accumulate predicate? combiner null-value term a next b)
  (if (> a b)
    null-value
    ;; (let) formulation:
    (let ((x (if (predicate? a) (term a) null-value)))
      (combiner x
                (filtered-accumulate 
                  predicate? combiner null-value term (next a) next b)))))
    ;; equivalent (lambda) formulation:
    ; ((lambda (x) (combiner x
    ;                        (filtered-accumulate 
    ;                          predicate? combiner null-value term (next a) next b)))
    ;  (if (predicate? a) (term a) null-value))))

; Sum of the squares of the prime numbers in the interval a to b
(define (sum-sq-prime a b)
  (filtered-accumulate prime? + 0 square a 1+ b))

; Test code:
(load "ex1_22-24.scm") ; get "prime?" function and helpers
(sum-sq-prime 0 10) ;Value: 88
(sum-sq-prime 0 15) ;Value: 378

; Product of all positive integers less than n that are relatively prime to n,
; i.e. all positive integers i < n s.t. GCD(i,n) == 1

; Use Euclid's algorithm (also in "../../test_code/gcd.scm")
(load "../../test_code/gcd.scm")
(define (prod-rel-prime n)
  (define (identity a) a)
  (define (rel-prime? a)
    (= (gcd a n) 1))
  (filtered-accumulate rel-prime? * 1 identity 1 1+ n))

; Test code:
(prod-rel-prime 10) ;Value: 189
;;==============================================================================
;;==============================================================================
