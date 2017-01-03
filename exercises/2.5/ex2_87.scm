;;==============================================================================
;;     File: ex2_87.scm
;;  Created: 01/02/2017, 14:10
;;   Author: Bernie Roesler
;;
;;  Description: Test polynomial procedures in polynomials.scm
;;
;;==============================================================================
(load "polynomials.scm")

;;; Terms are represented as: (order coeff)

;;; Ex 2.87 =zero?
(define p (make-polynomial 'x '((10 7) (5 1) (2 2))))
(define q (make-polynomial 'x '((10 0) (5 0) (2 0))))
(define z (make-polynomial 'x '()))
(printval (=zero? p)) ; Value: #f
(printval (=zero? q)) ; Value: #t
(printval (=zero? z)) ; Value: #t

;;==============================================================================
;;==============================================================================
