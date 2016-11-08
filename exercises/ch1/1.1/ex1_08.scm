;;==============================================================================
;;     File: ex1_8.scm
;;  Created: 10/31/2016, 21:39
;;   Author: Bernie Roesler
;;
;;  Description: exercise 1.8, cube roots
;;
;;==============================================================================
; Perform Newton's iteration to find the cube root of a number. Takes the initial
; <guess> and the number of which you are taking a square root <x>
(define (cubert-iter guess x)
  (if (good-enough-cubert? guess x) ; insert "new-if" in place of "if" here
    guess
    (cubert-iter (improve-cubert guess x) x)))

; Improve the guess by averaging it with the quotient of the radicand and the
; old guess
(define (improve-cubert guess x)
  (/ (+ (/ x (square guess)) 
        (* 2 guess))
     3))

(define (cube x)
  (* x x x))

(define (good-enough-cubert? guess x)
  (< (abs (- (cube guess) x)) tol))

; start routine
(define (cubert x)
  (cubert-iter 1.0 x))

; Test code
(cubert 27) ; Value: 3.0000005410641766
(cubert 8)  ; Value: 2.000004911675504
;;==============================================================================
;;==============================================================================
