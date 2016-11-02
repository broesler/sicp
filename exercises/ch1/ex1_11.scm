;;==============================================================================
;;     File: ex1_11.scm
;;  Created: 11/01/2016, 14:48
;;   Author: Bernie Roesler
;;
;;  Description: Recursion and iteration
;;
;;==============================================================================

; Consider the piecewise function:
;   f(n) = / n                         , if n < 3
;          \ f(n-1) + 2f(n-2) + 3f(n-3), if n >= 3

; Recursive process -- straightforward from function definition
(define (fr n)
  (if (< n 3)
    n
    (+ (fr (- n 1))
       (* 2 (fr (- n 2)))
       (* 3 (fr (- n 3))))))

; Test code
; (f -1) ; Value: -1
; (f  0) ; Value:  0
; (f  1) ; Value:  1
; (f  2) ; Value:  2
; (f  3) ; Value:  4
; (f  4) ; Value: 11
; (f  5) ; Value: 25
; (f 20) ; Value: 10771211 --> quite slow. 
; (f 30) impossibly slow.

; Iterative process -- keep a running sum as we count down from n to 0
; Initialize with proper values
(define (f n)
  (f-iter 2 1 0 n))

; iterate! need 1 special case, and 1 stopping case
(define (f-iter fn1 fn2 fn3 count)
  (cond ((< count 2) count)
        ((< count 3) fn1)
        (else (f-iter (+ fn1 (* 2 fn2) (* 3 fn3))
                      fn1
                      fn2
                      (- count 1)))))

; Test code
; (f -1) ; Value: -1
; (f  0) ; Value:  0
; (f  1) ; Value:  1
; (f  2) ; Value:  2
; (f  3) ; Value:  4
; (f  4) ; Value: 11
; (f  5) ; Value: 25
; (f 20) ; Value: 10771211 --> fast!
; (f 30) ; Value: 61354575194 --> computable!

;;==============================================================================
;;==============================================================================
