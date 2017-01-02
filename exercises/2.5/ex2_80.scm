;;==============================================================================
;;     File: ex2_80.scm
;;  Created: 12/15/2016, 18:05
;;   Author: Bernie Roesler
;;
;;  Description: Test =zero? procedure
;;
;;==============================================================================
(load "generic_arithmetic.scm")

;;; Test equality
(define a (make-scheme-number 1))
(define b (make-scheme-number 0))
(printval (=zero? a)) ; Value: #t
(printval (=zero? b)) ; Value: #f

(define a (make-rational 3 4))
(define b (make-rational 0 8))
(printval (=zero? a)) ; Value: #f
(printval (=zero? b)) ; Value: #t

(define a (make-complex-from-real-imag 1 1))
(define b (make-complex-from-real-imag 0 0))
(define c (make-complex-from-real-imag 0 1))
(printval (=zero? a)) ; Value: #f
(printval (=zero? b)) ; Value: #t
(printval (=zero? c)) ; Value: #f
;;==============================================================================
;;==============================================================================
