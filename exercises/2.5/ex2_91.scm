;;==============================================================================
;;     File: ex2_91.scm
;;  Created: 01/04/2017, 19:42
;;   Author: Bernie Roesler
;;
;;  Description: Include div-poly
;;
;;==============================================================================
(load "polynomials.scm")

(define p (make-polynomial 'x '((5 1) (0 -1))))
(define q (make-polynomial 'x '((2 1) (0 -1))))

;;; Test code:
(printval (add p q)) ; Value: (polynomial x (5 1) (2 1) (0 -2))
(printval (mul p q)) ; Value: (polynomial x (7 1) (5 -1) (2 -1) (0 1))
(printval (sub p q)) ; Value: (polynomial x (5 1) (2 -1))
(printval (div p q)) 
; Value: ((polynomial x (3 1) (1 1)) (polynomial x (1 1) (0 -1)))
; List of two polynomials...
;;==============================================================================
;;==============================================================================
