;;==============================================================================
;;     File: ex3_64.scm
;;  Created: 08/18/2017, 14:13
;;   Author: Bernie Roesler
;;
;;  Description: stream-limit 
;;
;;==============================================================================

(define (stream-limit s tol)
  (let ((a (stream-car s))
        (b (stream-cadr s)))
    (if (< (abs (- a b)) tol)
      b
      (stream-limit (stream-cdr s) tol))))

(define (sqrt-lim x tol)
  (stream-limit (sqrt-stream x) tol))

; (printval (abs (- (sqrt 2) (sqrt-lim 2 1e-6)))) ; Value: 2.220446049250313e-16

;;==============================================================================
;;==============================================================================
