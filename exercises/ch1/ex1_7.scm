;;==============================================================================
;;     File: ex1_7.scm
;;  Created: 10/31/2016, 21:40
;;   Author: Bernie Roesler
;;
;;  Description: Exercise 1.7, relative error in square roots
;;
;;==============================================================================

; Perform Newton's iteration to find the sqrt of a number. Takes the initial
; <guess> and the number of which you are taking a square root <x>
(define (sqrt-iter guess_old guess x)
  (if (good-enough? guess_old guess) ; insert "new-if" in place of "if" here
    guess
    (sqrt-iter guess (improve guess x) x)))

; Improve the guess by averaging it with the quotient of the radicand and the
; old guess
(define (improve guess x)
  (average guess (/ x guess)))

; Average two values
(define (average x y)
  (/ (+ x y) 2))

; What is good enough? Check for relative difference between guesses
(define tol 0.001)
(define (good-enough? guess_old guess)
  (< (/ (abs (- guess_old guess)) 
        guess_old) 
     tol))

; start routine
(define (mysqrt_rel x)
  (sqrt-iter 0.1 1.0 x))

; Ex 1.7 with relative error
; If tol = 0.001:
; (sqrt 16)       ; Value: 4.000000636692939
; (sqrt 2e-2)     ; Value: 0.14142135968022695
; (sqrt 4e200)    ; Value: 2e100 (works!!)

;;==============================================================================
;;==============================================================================
