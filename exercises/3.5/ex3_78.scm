;;==============================================================================
;;     File: ex3_78.scm
;;  Created: 09/04/2017, 20:33
;;   Author: Bernie Roesler
;;
;;  Description: solve 2nd order ODE
;;
;;==============================================================================
(load "streams.scm")

(define (solve-2 a b dy0 y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)

(define pi (* 4 (atan 1.0)))
(define N 1e2)
(stream-ref (solve-2 0 1 0 1 (/ pi N)) N)  ; Value:

;------------------------------------------------------------------------------- 
;        Ex 3.79 General solve-2
;-------------------------------------------------------------------------------
(define (solve-2 f dy0 y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

;;==============================================================================
;;==============================================================================
