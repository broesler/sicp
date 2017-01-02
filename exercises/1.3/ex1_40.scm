;;==============================================================================
;;     File: ex1_40.scm
;;  Created: 11/07/2016, 17:42
;;   Author: Bernie Roesler
;;
;;  Description: Cubic root-finding 
;;
;;==============================================================================

;------------------------------------------------------------------------------- 
;       Newton's method 
;-------------------------------------------------------------------------------
; (see test_code/fixed_points.scm)
; Derivative of a function (first forward difference)
(define (deriv g)
  (let ((dx 0.00001))
  (lambda (x) (/ (- (g (+ x dx)) (g x))
                 dx))))

; Newton's method: 
;  Find a fixed point of the mapping
;       x -> x - g(x)/g'(x)
;
(define (newton-map g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newton g guess)
  (fixed-point (newton-map g) guess))

; Use Newton's method for sqrt (1 works as initial guess for ANY x!)
(define (sqrt x)
  (newton (lambda (y) (- x (square y)))
          1.0))

;------------------------------------------------------------------------------- 
;       Ex 1.40 
;-------------------------------------------------------------------------------
; Define a procedure that can be used with Newton's method to approximate the
; zeros of the cubic:
;   x^3 + ax^2 + bx + c
;
(define (cubic a b c)
  (define (cube x) (* x x x))
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

; Test code:
((cubic -2 -1 2) 0) ;Value: 2

; Need multiple guesses to get each root!
(newline)
(display (newton (cubic -2 -1 2) 0.9))
(newline)
(display (newton (cubic -2 -1 2) 2.9))
(newline)
(display (newton (cubic -2 -1 2) -1.1))
;;==============================================================================
;;==============================================================================
