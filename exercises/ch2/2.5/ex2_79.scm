;;==============================================================================
;;     File: ex2_79.scm
;;  Created: 12/15/2016, 17:49
;;   Author: Bernie Roesler
;;
;;  Description: Test equ? procedure
;;
;;==============================================================================
(load "generic_arithmetic.scm")

;;; Test equality
(define a (make-scheme-number 3))
(define b (make-scheme-number 4))
(printval (equ? a a)) ; Value: #t
(printval (equ? a b)) ; Value: #f

(define a (make-rational 3 4))
(define b (make-rational 4 5))
(printval (equ? a a)) ; Value: #t
(printval (equ? a b)) ; Value: #f

(define a (make-complex-from-real-imag 1 1))
(define b (make-complex-from-real-imag 1 2))
(printval (equ? a a)) ; Value: #t
(printval (equ? a b)) ; Value: #f
;;==============================================================================
;;==============================================================================
