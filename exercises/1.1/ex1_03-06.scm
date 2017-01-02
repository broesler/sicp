;;==============================================================================
;;     File: sec1.scm
;;  Created: 10/31/2016, 17:22
;;   Author: Bernie Roesler
;;
;;  Description: Responses to exercises in Ch 1.1 os SCIP
;;
;;==============================================================================

;------------------------------------------------------------------------------- 
;        Ex 1.3
;-------------------------------------------------------------------------------
(define (sum-sq-max x y z)
  (cond ((and (< x y) (< x z)) (+ (square y) (square z)))
        ((and (< y x) (< y z)) (+ (square x) (square z)))
        (else (+ (square x) (square y)))))

; Test code:
(sum-sq-max 1 2 3)  ; Value: 13
(sum-sq-max 3 1 2)  ; Value: 13
(sum-sq-max 2 3 1)  ; Value: 13

;------------------------------------------------------------------------------- 
;       Ex 1.4
;-------------------------------------------------------------------------------
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; Test code
(a-plus-abs-b 3  2) ; Value: 5
(a-plus-abs-b 3 -2) ; Value: 5

;------------------------------------------------------------------------------- 
;       Ex 1.5
;-------------------------------------------------------------------------------
; (define (p) (p))
; (define (test x y)
;   (if (= x 0) 0 y))

; Test code
; (test 0  p ) ; Value: 0
; (test 0 (p)) ; Value: fail

;------------------------------------------------------------------------------- 
;       Ex 1.6
;-------------------------------------------------------------------------------
; new if-statement in terms of cond
(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
          (else else-clause)))

; Test code
; (new-if (= 2 3) 0 5) ; Value: 5
; (new-if (= 1 1) 0 5) ; Value: 0

; Perform Newton's iteration to find the sqrt of a number. Takes the initial
; <guess> and the number of which you are taking a square root <x>
(define (sqrt-iter guess x)
  (if (good-enough? guess x) ; insert "new-if" in place of "if" here
    guess
    (sqrt-iter (improve guess x) x)))

; If we used "new-if" in place of the special form "if", ALL of the arguments
; (i.e. the predicate, then-clause and else-clause) are evaluated *before*
; passing to "new-if". Thus, "sqrt-iter" is called recursively, which calls
; "new-if" again, which attempts to evaluate "sqrt-iter" again, etc. onto
; infinity (or the bottom of the recursion depth). The two recursions of
; "new-if" and "sqrt-iter" get locked in a never-ending tussle.

; Improve the guess by averaging it with the quotient of the radicand and the
; old guess
(define (improve guess x)
  (average guess (/ x guess)))

; Average two values
(define (average x y)
  (/ (+ x y) 2))

; What is good enough? Just check for difference between square of guess and x
; (define tol 0.001)
(define tol 0.001)
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) tol))

; start routine
(define (mysqrt_abs x)
  (sqrt-iter 1.0 x))

; Ex 1.7: rewrite "good-enough?" to use, say, (guess(i) - guess(i-1))/guess(i-1)
; If tol = 0.001:
; (mysqrt_abs 16)       ; Value: 4.000000636692939
; (mysqrt_abs 2e-2)     ; Value: 0.1444238094866232 (only accurate to 2 places)

;;==============================================================================
;;==============================================================================
