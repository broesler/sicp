;;==============================================================================
;;     File: ex2_56.scm
;;  Created: 11/14/2016, 15:39
;;   Author: Bernie Roesler
;;
;;  Description: Extending differentiation
;;
;;==============================================================================
(load "deriv.scm")

;;; Extend deriv to handle the rule:
;;;   d(u^n)/dx = n*u^(n-1)*(du/dx)
;;; including rules that x^0 = 1, and x^1 = x.

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
        ;; New exponentiation? clause here:
        ((exponentiation? expr)
         (make-product
           (make-product (exponent expr)
                         (make-exponentiation 
                           (base expr) 
                           (- (exponent expr) 1)))
           (deriv (base expr) var)))
        (else
         (error "unknown expression type -- DERIV" expr))))

;------------------------------------------------------------------------------- 
;        Define helpers
;-------------------------------------------------------------------------------
;;; Define '(** b n) as an exponent term

;; Predicate
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

;; Constructor
(define (make-exponentiation b n) 
  (cond ((=number? b 0) 0) ; 0^n = 0
        ((=number? n 0) 1) ; x^0 = 1
        ((=number? n 1) b) ; x^1 = x
        ((and (number? b) (number? n)) (exp b n))
        (else (list '** b n))))

;; Selectors
(define (base x) (cadr x))
(define (exponent x) (caddr x))

;;; Test code:
(printval (deriv '(** x 0) 'x)) ; Value: 0
(printval (deriv '(** x 1) 'x)) ; Value: 1
(printval (deriv '(** x 2) 'x)) ; Value: (* 2 x)
; (printval (deriv '(** x y) 'x)) ; ERROR --> only numbers allowed as expts
;;==============================================================================
;;==============================================================================
