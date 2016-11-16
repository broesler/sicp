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
                           (make-sum (exponent expr) '-1))) ; only numbers as exps!!!
           (deriv (base expr) var)))
        (else
         (error "unknown expression type -- DERIV" expr))))

;------------------------------------------------------------------------------- 
;        Ex 2.56 Define exponentiation
;-------------------------------------------------------------------------------
;;; Define '(** b n) as an exponent term

;; Predicate
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

;; Constructor
(define (make-exponentiation b n) 
  (cond ((=number? b 0) 0) ; 0^n = 0
        ((=number? b 1) 1) ; 1^n = 1
        ((=number? n 0) 1) ; x^0 = 1
        ((=number? n 1) b) ; x^1 = x
        ((and (number? b) (number? n)) (exp b n))
        (else (list '** b n))))

;; Selectors
(define (base x) (cadr x))
(define (exponent x) (caddr x))

;;; Test code:
(newline)
(display "Ex 2.56 -- Exponentiation:")
(printval (deriv '(** x 0) 'x)) ; Value: 0
(printval (deriv '(** x 1) 'x)) ; Value: 1
(printval (deriv '(** x 2) 'x)) ; Value: (* 2 x)
(printval (deriv '(** x y) 'x)) ; Value: (* y (** x (+ y -1)))

;------------------------------------------------------------------------------- 
;        Ex 2.57
;-------------------------------------------------------------------------------
;;; Handle sums and products of arbitrary numbers of terms
;;; i.e. (deriv '(* x y (+ x 3)) 'x) 
(load "../2.2/sequence_operations.scm") ; get accumulate

;;; Redefine sum selectors
; (define (addend s) (cadr s))
(define (augend s) 
  (accumulate make-sum 0 (cddr s)))

;;; Redefine multiply selectors
; (define (multiplier p) (cadr p))
(define (multiplicand p) 
  (accumulate make-product 1 (cddr p)))

;;; Test code:
(newline)
(display "Ex 2.57 -- Arbitrary terms:")
(printval (deriv '(+ 3 y (** x 2)) 'x))    ; Value: (* 2 x)
(printval (deriv '(* (* x y) (+ x 3)) 'x)) ; Value: (+ (* x y) (* y (+ x 3)))
(printval (deriv '(* x y (+ x 3)) 'x))     ; Value: (+ (* x y) (* y (+ x 3)))
;==============================================================================
;==============================================================================
