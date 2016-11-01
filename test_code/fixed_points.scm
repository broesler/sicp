;;==============================================================================
;;     File: fixed_points.scm
;;  Created: 10/28/2016, 15:00
;;   Author: Bernie Roesler
;;
;;  Description: Compute the fixed points of a function
;;==============================================================================

; (define (sqrt x)
;   (fixed-point
;     (lambda (y) (average (/ x y) y)) ; how do we know this will converge!?
;     1)
;   )
;
; (define (sqrt x)
;   (fixed-point 
;     (average-damp (lambda (y) (/ x y)))
;     1))

; Use Newton's method
(define (sqrt x)
  (newton (lambda (y) (- x (square y)))
          1))

; Newton's method: y(i+1) = y(i) + f(x) / df/dx(x)
(define (newton f guess)
  (define df (deriv f)) ; define deriv here so it doesn't get recomputed each
                        ; time we call fixed-point
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
(define dx 1e-5)

; Get rid of oscillating solutions
(define average-damp
  (lambda (f)
    (lambda (x) (average (f x) x))))

; Find a point where g(x) = x
(define (fixed-point f start)
  (define (iter old new)
    (if (close-enough? old new)
      new
      (iter new (f new))))
  (iter start (f start)))

;;==============================================================================
;;==============================================================================
