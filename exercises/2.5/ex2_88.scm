;;==============================================================================
;;     File: ex2_88.scm
;;  Created: 01/03/2017, 14:41
;;   Author: Bernie Roesler
;;
;;  Description: Include sub-poly, and generic negation
;;
;;==============================================================================
(load "polynomials.scm")

(define p (make-polynomial 'x '((10 7) (5 1) (2 2))))
(define q (make-polynomial 'x '((10 2) (5 2) (2 4) (0 3))))

;;; Test generic negate:
(printval (negate a)) ; Value: -7
(printval (negate b)) ; Value: (-3 . 4)
(printval (negate c)) ; Value: -3.5
(printval (negate d)) ; Value: (complex rectangular -1 . -1)
(printval (negate p)) ; Value: (polynomial x (10 -7) (5 -1) (2 -2))

(printval (add p q)) 
; Value: (polynomial x (10 9) (5 3) (2 6) (0 3))
(printval (mul p q)) 
; Value: (polynomial x (20 14) (15 16) (12 32) (10 23) (7 8) (5 3) (4 8) (2 6))
(printval (sub p q)) 
; Value: (polynomial x (10 5) (5 -1) (2 -2) (0 -3))

;;==============================================================================
;;==============================================================================
