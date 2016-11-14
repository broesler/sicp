;;==============================================================================
;;     File: deriv.scm
;;  Created: 11/14/2016, 15:24
;;   Author: Bernie Roesler
;;
;;  Description: Symbolic differentiation 
;;
;;==============================================================================

;;; General differentiation expression
(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ((product? expr)
         (make-sum
           (make-product (multiplier expr)
                         (deriv (multiplicand expr) var))
           (make-product (deriv (multiplier expr) var)
                         (multiplicand expr))))
        (else
         (error "unknown expression type -- DERIV" expr))))

;------------------------------------------------------------------------------- 
;        Define these subprocedures:
;-------------------------------------------------------------------------------
;;; (variable? e)               Is e a variable?
;;; (same-variable? v1 v2)      Are v1 and v2 the same variable?
;;; (sum? e)                    Is e a sum?
;;; (addend e)                  Addend of the sum e.
;;; (augend e)                  Augend of the sum e.
;;; (make-sum a1 a2)            Construct the sum of a1 and a2.
;;; (product? e)                Is e a product?
;;; (multiplier e)              Multiplier of the product e.
;;; (multiplicand e)            Multiplicand of the product e.
;;; (make-product m1 m2)        Construct the product of m1 and m2.

;;; Using prefix notation for expr, i.e. ax+b ==> (+ (* a x) b)

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

;;; 2nd and 3rd items of list
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

;;; 2nd and 3rd items of list
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

;------------------------------------------------------------------------------- 
;        Test code:
;-------------------------------------------------------------------------------
(printval (deriv '(+ x 3) 'x)) 
(printval (deriv '(* x y) 'x)) 
(printval (deriv '(* (* x y) (+ x 3)) 'x)) 

;;; Not-simplified version of above:
; Value: (+ 1 0)
; Value: (+ (* x 0) (* 1 y))
; Value: (+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))

;------------------------------------------------------------------------------- 
;         Update code for simplification
;-------------------------------------------------------------------------------
;;; Update make-sum and make-product constructors
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

;; check if an expression is equal to a number
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;;; Test code:
(printval (deriv '(+ x 3) 'x)) 
(printval (deriv '(* x y) 'x)) 
(printval (deriv '(* (* x y) (+ x 3)) 'x)) 
;; Simplified now!
; Value: 1
; Value: y
; Value: (+ (* x y) (* y (+ x 3)))

;;==============================================================================
;;==============================================================================
