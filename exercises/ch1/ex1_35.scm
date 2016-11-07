;;==============================================================================
;;     File: ex1_35.scm
;;  Created: 11/06/2016, 17:09
;;   Author: Bernie Roesler
;;
;;  Description: fixed points
;;
;;==============================================================================

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

; Test code
; (fixed-point cos 1.0) ;Value: 0.7390547907469174
; (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0) ;Value: 1.2586974741689445

; Golden ratio! x -> 1 + 1/x (i.e. a root of x^2 - x - 1 = 0)
(fixed-point (lambda (x) (+ 1.0 (/ 1.0 x))) 1.0) ;Value: 1.6180555555555556

;;==============================================================================
;;==============================================================================
