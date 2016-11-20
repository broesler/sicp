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
  (cond ((constant? expr) 0)
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
;;; Using prefix notation for expr, i.e. ax+b ==> (+ (* a x) b)

;;; Is x a constant?
(define (constant? x) (number? x))

;;; Is x a variable?
(define (variable? x) (symbol? x))

;;; Are v1 and v2 the same variable?
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;;; Construct the sum or product of a1 and a2
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))

;;; Is x a sum?
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

;;; Is x a product?
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

;;; Addend and augend of the sum s (2nd and 3rd items of list)
(define (addend s) (cadr s))
(define (augend s) (caddr s))

;;; Multiplier and multiplicand of the product p (2nd and 3rd items of list)
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

;;;;;;;;;; Test code:
;;; From lecture:
; (define foo                 ; a*x*x + b*x + c
;   '(+ (* a (* x x)) 
;       (+ (* b x) 
;          c)))
; (printval (deriv foo 'x))
; Value:
; (+ (+ (* a (+ (* x 1) (* 1 x))) 
;       (* 0 (* x x))) 
;    (+ (+ (* b 1) (* 0 x)) 
;       0))                               ; Not simplified!!

;;; From book:
; (printval (deriv '(+ x 3) 'x)) 
; (printval (deriv '(* x y) 'x)) 
; (printval (deriv '(* (* x y) (+ x 3)) 'x)) 

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
        ((and (number? a1) 
              (number? a2)) 
         (+ a1 a2))
        (else (list '+ a1 a2))))

;; check if an expression is equal to a number
(define (=number? expr num)
  (and (number? expr) (= expr num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;;;;;;;;;; Test code:
; (printval (deriv '(+ x 3) 'x)) 
; (printval (deriv '(* x y) 'x)) 
; (printval (deriv '(* (* x y) (+ x 3)) 'x)) 
;; Simplified now!
; Value: 1
; Value: y
; Value: (+ (* x y) (* y (+ x 3)))

;;==============================================================================
;;==============================================================================
