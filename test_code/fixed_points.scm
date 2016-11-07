;;==============================================================================
;;     File: fixed_points.scm
;;  Created: 10/28/2016, 15:00
;;   Author: Bernie Roesler
;;
;;  Description: Compute the fixed points of a function
;;==============================================================================

; Lecture notes:
; (define (sqrt x)
;   (fixed-point
;     (lambda (y) (average (/ x y) y)) ; how do we know this will converge!?
;     1))

; Average two values
; (define (average x y)
;   (/ (+ x y) 2.0))

; Find a point where g(x) = x
(define (fixed-point f start)
  (define tol 0.0001)
  (define (close-enough? u v)
    (< (abs (- u v)) tol))
  (define (iter old new)
    (if (close-enough? old new)
      new
      (iter new (f new))))
  (iter start (f start)))

; Redefine sqrt with average damping of y |--> x/y
; (define (sqrt x)
;   (fixed-point 
;     (average-damp (lambda (y) (/ x y)))
;     1))

; NOTE "average-damp" is a procedure which takes a procedure as its argument,
; and returns a procedure which takes one argument
; (define average-damp
;   (lambda (f) 
;     (lambda (x) (average (f x) x))))

; "I am defining 'average-damp' to name a procedure that takes a procedure as
; its one argument (f), and returns a procedure that takes a number as its one
; argument (x), and returns a number (i.e. the average of f(x) and x)"

; Use Newton's method for sqrt (1 works as initial guess for ANY x!)
(define (sqrt x)
  (newton (lambda (y) (- x (square y)))
          1.0))

; Newton's method: y(i+1) = y(i) + f(x) / df/dx(x)
(define (newton f guess)
  (define df (deriv f)) ; define deriv here so it doesn't get recomputed
  (fixed-point (lambda (x) (- x (/ (f x) (df x))))
               guess))

; Derivative of a function
(define (deriv f)
  (lambda (f)
    (lambda (x)
      (/ (- (f (+ x dx))
            (f x))
         dx))))

; Step-size
(define dx 0.00001)

; Test code
(sqrt 2) ;Value: 
;;==============================================================================
;;==============================================================================
