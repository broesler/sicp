;;==============================================================================
;;     File: ex2_73.scm
;;  Created: 11/25/2016, 16:10
;;   Author: Bernie Roesler
;;
;;  Description: Derivatives as a data-directed system 
;;
;;==============================================================================
;;; Constuctor
(define (deriv expr var)
  (cond ((constant? expr) 0)
        ((variable? expr) 
         (if (same-variable? expr var) 1 0))
        (else 
          ((get 'deriv (operator expr)) (operands expr)
                                        var))))

;;; Selectors
(define (operator expr) (car expr))
(define (operands expr) (cdr expr))

;;; Predicates
(define (constant? x) (number? x))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;------------------------------------------------------------------------------- 
;        (a) Explain how it works
;-------------------------------------------------------------------------------
;;; We take the 'type tag' to be the algebraic operator ('+ i.e.), so we cannot
;;; assimilate the 'constant?' and 'variable?' predicates because they do not
;;; have an operator associated with them.
;;;
;;; Note that we can get rid of all the predicates, because the (get) operation
;;; takes care of them for us!

;------------------------------------------------------------------------------- 
;        (b) Sums and products + installation code
;-------------------------------------------------------------------------------
(define (install-sum-package)
  ;;---------- Internal procedures ----------
  ;; Constructor
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) 
                (number? a2)) 
           (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (=number? expr num)
    (and (number? expr) (= expr num)))

  ;; Selectors
  (define (addend s) (cadr s))
  (define (augend s) (caddr s))

  ;; Take the derivative of a sum
  (define (deriv expr) 
    (make-sum (deriv (addend expr) var)
              (deriv (augend expr) var)))

  ;;---------- Interface to rest of system ----------
  ;; (put <op> <type> <item>)
  (put 'deriv '(+) deriv)
  'done)

(define (install-prod-package)
  ;;---------- Internal procedures ----------
  ;; Constructor
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

  ;; Selectors
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (caddr p))

  ;; Take the derivative of a product
  (define (deriv expr)
    (make-sum
      (make-product (multiplier expr)
                    (deriv (multiplicand expr) var))
      (make-product (deriv (multiplier expr) var)
                    (multiplicand expr))))

  ;;---------- Interface to rest of system ----------
  (put 'deriv '(*) deriv)
  'done)

;;;;;;;;;; Test code:
; (install-sum-package)
; (install-prod-package)
; (define foo                 ; a*x*x + b*x + c
;   '(+ (* a (* x x)) 
;       (+ (* b x) 
;          c)))
; (printval (deriv foo 'x))

;------------------------------------------------------------------------------- 
;        (c) include exponent
;-------------------------------------------------------------------------------
(define (install-exp-package)
  ;;; NOTE: assumes that (make-sum) and (make-product) are already available!
  ;;---------- Internal procedures ----------
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

  ;;; Take the derivative of an exponent
  (define (deriv expr)
    (make-product
      (make-product (exponent expr)
                    (make-exponentiation 
                      (base expr) 
                      (make-sum (exponent expr) '-1)))
      (deriv (base expr) var)))

  ;;---------- Interface to rest of system ----------
  (put 'deriv '(**) deriv)
  'done)

;;;;;;;;;; Test code:
; (install-exp-package)
; (printval (deriv '(** x 2) 'x)) ; Value: (* 2 x)

;------------------------------------------------------------------------------- 
;        (d) opposite indexing?
;-------------------------------------------------------------------------------
;;; ((get (operator expr) 'deriv) (operands expr) var)
;;;   vs.
;;; ((get 'deriv (operator expr)) (operands expr))
;;; 
;;; The only change we have to make is switching the order of the (put ...)
;;; commands above! So <type> is always 'deriv, and <op> is '+, '*, '**, ...


;;==============================================================================
;;==============================================================================
