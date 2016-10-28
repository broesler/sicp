;;==============================================================================
;;     File: sqrt.scm
;;  Created: 10/27/2016, 21:58
;;   Author: Bernie Roesler
;;
;;  Description: 
;;
;;==============================================================================

; Define new-if using cond
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; Perform Newton's iteration to find the sqrt of a number. Takes the initial
; <guess> and the number of which you are taking a square root <x>
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
    x)))

; Improve the guess by averaging it with the quotient of the radicand and the
; old guess
(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2)

; What is good enough? Just check for tolerance of square
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001)

; start routine
(define (sqrt x)
  (sqrt-iter 1.0 x))

;;==============================================================================
;;==============================================================================
