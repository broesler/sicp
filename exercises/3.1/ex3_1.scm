;;==============================================================================
;;     File: ex3_1.scm
;;  Created: 01/11/2017, 10:33
;;   Author: Bernie Roesler
;;
;;  Description: accumulators
;;
;;==============================================================================

(define (make-accumulator init)
  (let ((sum init))
    (lambda (amt)
      (set! sum (+ sum amt))
      sum)))

;;; Test code:
(define A (make-accumulator 5))
(printval (A 10)) ; Value: 15
(printval (A 10)) ; Value: 25

;;==============================================================================
;;==============================================================================
