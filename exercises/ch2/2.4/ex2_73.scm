;;==============================================================================
;;     File: ex2_73.scm
;;  Created: 11/25/2016, 16:10
;;   Author: Bernie Roesler
;;
;;  Description: Derivatives as a data-directed system 
;;
;;==============================================================================
(load "../../../sicp_code/ch2support.scm") ;; for put/get operations
(load "complex.scm") ;; for (attach-tag)

;;; Constuctor
(define (deriv expr var)
  (cond ((constant? expr) 0)
        ((variable? expr) 
         (if (same-variable? expr var) 1 0))
        (else ((get 'deriv (operator expr)) (operands expr)
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
;;; Compare expression to number
(define (=number? expr num)
  (and (number? expr) (= expr num)))

(define (install-sum-package)
  ;;---------- Internal procedures ----------
  ;; Constructor
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) 
                (number? a2)) 
           (+ a1 a2))
          (else (list '+ a1 a2)))) ; NOTE: "tag" included here!

  ;; Selectors (only take 2 operators, NOT complete expression)
  (define (addend s) (car s))
  (define (augend s) (cadr s))

  ;;---------- Interface to rest of system ----------
  ;; (put <op> <type> <item>)
  ;; NOTE: Do not put "'(+)" if we are using (get) above. List form only
  ;; works with (apply-generic)
  ;; Derivative procedure:
  (put 'deriv '+ 
       (lambda (opers var) 
         (make-sum (deriv (addend opers) var)
                   (deriv (augend opers) var))))
  (put 'make-sum '+
       (lambda (a b) (make-sum a b)))
  'done)

;;;;;;;;;; Test code:
(install-sum-package)
(printval (deriv '(+ x 2) 'x)) ; Value: 1

(define (install-prod-package)
  ;;---------- Internal procedures ----------
  ;; Requires sum-package
  (define (make-sum a b)
    ((get 'make-sum '+) a b))

  ;; Constructor
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2)))) ; NOTE: "tag" included here!

  ;; Selectors (only take 2 operators, NOT complete expression)
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))

  ;;---------- Interface to rest of system ----------
  ;; Derivative procedure:
  (put 'deriv '* 
       (lambda (opers var)
         (make-sum
           (make-product (multiplier opers)
                         (deriv (multiplicand opers) var))
           (make-product (deriv (multiplier opers) var)
                         (multiplicand opers)))))
  (put 'make-product '*
       (lambda (a b) (make-product a b)))
  'done)

;;;;;;;;;; Test code:
(install-prod-package)
(printval (deriv '(* x y) 'x))
(define foo                 ; a*x*x + b*x + c
  '(+ (* a (* x x)) 
      (+ (* b x) 
         c)))
(printval (deriv foo 'x)) ; Value: (+ (* a (+ x x)) b)

;------------------------------------------------------------------------------- 
;        (c) include exponent
;-------------------------------------------------------------------------------
(define (install-exp-package)
  ;;---------- Internal procedures ----------
  (define (make-sum a b) 
    ((get 'make-sum '+) a b))
  (define (make-product a b) 
    ((get 'make-product '*) a b))

  ;; Constructor
  (define (make-exponentiation b n) 
    (cond ((=number? b 0) 0) ; 0^n = 0
          ((=number? b 1) 1) ; 1^n = 1
          ((=number? n 0) 1) ; x^0 = 1
          ((=number? n 1) b) ; x^1 = x
          ((and (number? b) (number? n)) (exp b n))
          (else (list '** b n))))

  ;; Selectors
  (define (base x) (car x))
  (define (exponent x) (cadr x))

  ;;; Take the derivative of an exponent

  ;;---------- Interface to rest of system ----------
  (put 'deriv '** 
       (lambda (expr var)
         (make-product
           (make-product (exponent expr)
                         (make-exponentiation 
                           (base expr) 
                           (make-sum (exponent expr) '-1)))
           (deriv (base expr) var))))
  (put 'make-exponentiation '**
       (lambda (b n) (make-exponentiation b n)))
  'done)

;;;;;;;;;; Test code:
(install-exp-package)
(define foo                 ; a*x^2 + b*x + c
  '(+ (* a (** x 2)) 
      (+ (* b x) 
         c)))
(printval (deriv foo 'x)) ; Value: (+ (* a (* 2 x)) b)

;------------------------------------------------------------------------------- 
;        (d) opposite indexing?
;-------------------------------------------------------------------------------
;;; ((get (operator expr) 'deriv) (operands expr) var)
;;;   vs.
;;; ((get 'deriv (operator expr)) (operands expr))
;;; 
;;; The only change we have to make is switching the order of the (put ...)
;;; commands above! So <type> is always 'deriv, and <op> is '+, '*, '**, ...
;;; i.e. (put 'deriv '(+) deriv) ==> (put '+ '(deriv) deriv)
;;==============================================================================
;;==============================================================================
