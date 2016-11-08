;;==============================================================================
;;     File: ex1_41.scm
;;  Created: 11/07/2016, 17:59
;;   Author: Bernie Roesler
;;
;;  Description: Procedure double that applies a procedure twice 
;;
;;==============================================================================

; Takes a procedure of one argument as its argument
(define (double f)
  (lambda (x) (f (f x))))

; Test code:
(define (inc x) (+ x 1))

((double inc) 5) ; Value: 7
((double (double inc)) 5) ; Value: 9
(((double (double double)) inc) 5) ; Value: 21

;;==============================================================================
;;==============================================================================
