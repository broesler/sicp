;;==============================================================================
;;     File: sqrt.scm
;;  Created: 10/27/2016, 21:58
;;   Author: Bernie Roesler
;;
;;  Description: 
;;
;;==============================================================================

; Perform Newton's iteration to find the sqrt of a number. Takes the initial
; <guess> and the number of which you are taking a square root <x>
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)
    )
  )

; Improve the guess by averaging it with the quotient of the radicand and the
; old guess
(define (improve guess x)
  (average guess (/ x guess)))

; Average two values
(define (average x y)
  (/ (+ x y) 2))

; What is good enough? Just check for tolerance of square
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

; start routine
(define (sqrt x)
  (sqrt-iter 1.0 x))

;------------------------------------------------------------------------------- 
;        Redefine block structure
;-------------------------------------------------------------------------------
; NOTE Remove 'x' from each argument list, use *lexical scoping*
; (define (sqrt x)
;   (define (good-enough? guess)
;     (< (abs (- (square guess) x)) 0.001))
;   (define (improve guess)
;     (average guess (/ x guess)))
;   (define (sqrt-iter guess)
;     (if (good-enough? guess)
;         guess
;         (sqrt-iter (improve guess))))
;   (sqrt-iter 1.0))
;;==============================================================================
;;==============================================================================
