;;==============================================================================
;;     File: ex1_46.scm
;;  Created: 11/07/2016, 19:05
;;   Author: Bernie Roesler
;;
;;  Description: iterative improvement
;;
;;==============================================================================

; Take two procedures as arguments:
;   good-enough?    -- method for telling whether a guess is good enough
;   improve         -- method for improving a guess
; return: a procedure that takes a guess as argument and keeps improving until
; the guess is good enough
(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
      guess
      (iter (improve guess))))
  iter)

; Rewrite sqrt from 1.1.7 in terms of iterative-improve
(define (sqrt x)
  (define tol 0.000001)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) tol))
  (define (improve guess)
    (average guess (/ x guess)))
  ; evaluate iterative-improve to get a procedure that iterates on the guess,
  ; and then call that procedure with 1.0 as a guess
  ((iterative-improve good-enough? improve) 1.0))

; Test code:
(newline)
(display (sqrt 2)) ; Value: 

; Rewrite fixed-point from 1.3.3 in terms of iterative-improve
(define (fixed-point f start)
  (define tol 0.0001)
  (define (close-enough? guess)
    (< (abs (- (f guess) guess)) tol))
  ; generate a procedure that improves start by repeatedly applying f
  ((iterative-improve close-enough? f) start))

(newline)
(display (fixed-point cos 1.0)) ; Value: 0.7391301765296711
;;==============================================================================
;;==============================================================================
