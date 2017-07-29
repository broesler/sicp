;;==============================================================================
;;     File: ex2_58.scm
;;  Created: 11/14/2016, 18:45
;;   Author: Bernie Roesler
;;
;;  Description: Modify differentiation for ordinary math notation (infix)
;;
;;==============================================================================
(load "deriv.scm")

;------------------------------------------------------------------------------- 
;        (a) infix notation fully parenthesized
;-------------------------------------------------------------------------------
;;; assume + and * always have 2 arguments and expr is fully parenthesized 
;;; only have to change the predicates, selectors, and constructors!!
;;; Predicates:
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+))) ; Second symbol is operator
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

;;; Constructors:
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) 
              (number? a2)) 
         (+ a1 a2))
        (else (list a1 '+ a2)))) ; '+ goes in the middle

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

;;; Selectors:
(define (addend s) (car s))
(define (augend s) (caddr s)) ; third symbol is augend (same as before)

(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

;;; Test code:
(define foo '(x + (3 * (x + ((y * y) + 2))))) ; x + 3*(x + y*y + 2)
(printval (deriv foo 'x)) ; Value: 4
(printval (deriv foo 'y)) ; Value: (3 * (y + y))
;;==============================================================================
;;==============================================================================
