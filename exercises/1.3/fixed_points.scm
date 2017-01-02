;;==============================================================================
;;     File: fixed_points.scm
;;  Created: 10/28/2016, 15:00
;;   Author: Bernie Roesler
;;
;;  Description: Compute the fixed points of a function
;;==============================================================================

; Section 1.3.4 + Lecture Notes (2A)
; (define (sqrt x)
;   (fixed-point
;     (lambda (y) (average (/ x y) y)))
;   1.0) ; how do we know this will converge!? No damping!

;------------------------------------------------------------------------------- 
;        Find a point where g(x) = x
;-------------------------------------------------------------------------------
(define (fixed-point f start)
  (define tol 0.0001)
  (define (close-enough? u v)
    (< (abs (- u v)) tol))
  (define (iter old new)
    (if (close-enough? old new)
      new
      (iter new (f new))))
  (iter start (f start)))

; NOTE "average-damp" is a procedure which takes a procedure as its argument,
; and returns a procedure which takes one argument
(define (average-damp f)
  (define (average x y)
    (/ (+ x y) 2.0))
  (lambda (x) (average (f x) x)))

; Test code:
; ((average-damp square) 10) ;Value: 55.0 (avg of x + x^2)

; "I am defining 'average-damp' to name a procedure that takes a procedure as
; its one argument (f), and returns a procedure that takes a number as its one
; argument (x), and returns a number (i.e. the average of f(x) and x)"

;------------------------------------------------------------------------------- 
;        Redefine sqrt with average damping of y |--> x/y
;-------------------------------------------------------------------------------
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 
               1.0))

; Test code:
(newline)
(display (sqrt 2)) ; Value: 1.4142135623746899

;------------------------------------------------------------------------------- 
;        cube roots
;-------------------------------------------------------------------------------
(define (cube-root x)
  ;; Use either of these lines:
  ; (define f (lambda (y) (/ x (square y))))
  ; (define (f y) (/ x (square y)))
  ;; with this line:
  ; (fixed-point (average-damp f)
  ;; Or write it the compact way:
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

; Test code:
(newline)
(display (cube-root 8)) ; Value: 1.999970920454376

;------------------------------------------------------------------------------- 
;       Newton's method 
;-------------------------------------------------------------------------------
; Derivative of a function (first forward difference)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x))
                 dx)))

; Step-size
(define dx 0.00001)

; Test code
(define (cube x) (* x x x))
(newline)
(display ((deriv cube) 5)) ; Value: 75.00014999664018

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

; Test code:
(newline)
(display (sqrt 2)) ; Value: 1.4142135623822438

;------------------------------------------------------------------------------- 
;       First-class procedures 
;-------------------------------------------------------------------------------
; General idea of finding a fixed point of a transformation of a function
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

; Recast sqrt computation (with average damp)
(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

; Recast again with Newton's method
(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-map
                            1.0))
;;==============================================================================
;;==============================================================================
