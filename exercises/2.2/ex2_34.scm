;;==============================================================================
;;     File: ex2_34.scm
;;  Created: 11/12/2016, 18:56
;;   Author: Bernie Roesler
;;
;;  Description: Horner's Rule implementation
;;
;;==============================================================================

;;; Horner's rule evaluates by 
;;;     (a_n*x + a_{n-1})*x + \dots + a_1)*x + a_0
;;; In other words, we start with a_n, multiply by x, add a_{n-1}, multiply by
;;; x, and so on, until we reach a_0.

;;; Horner-eval
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;; For example, to compute 1 + 3x + 5x^3 + x^5 at x = 2 you would evaluate
(printval (horner-eval 2 (list 1 3 0 5 0 1))) ; Value: 79
;;==============================================================================
;;==============================================================================
