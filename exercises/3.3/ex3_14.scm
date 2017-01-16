;;==============================================================================
;;     File: ex3_14.scm
;;  Created: 01/14/2017, 13:23
;;   Author: Bernie Roesler
;;
;;  Description: mystery loop!
;;
;;==============================================================================

;;; mystery procedure
(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (cdr x)))
        (set-cdr! x y)
        (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(printval v) ; Value: (a b c d)
(define w (mystery v))
(printval v) ; Value: (a)
(printval w) ; Value: (d c b a)
;;==============================================================================
;;==============================================================================
