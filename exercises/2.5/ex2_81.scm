;;==============================================================================
;;     File: ex2_81.scm
;;  Created: 12/19/2016, 22:13
;;   Author: Bernie Roesler
;;
;;  Description: 
;;
;;==============================================================================
(load "generic_arithmetic.scm")

;;; 2.5.2 apply-generic definition
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (let ((t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1)))
              (cond (t1->t2
                      (apply-generic op (t1->t2 a1) a2))
                    (t2->t1
                      (apply-generic op a1 (t2->t1 a2)))
                    (else
                      (error "No method for these types"
                             (list op type-tags))))))
          (error "No method for these types"
                 (list op type-tags)))))))

;;; Louis Reasoner argues that we need:
; (define (scheme-number->scheme-number n) n)
; (define (complex->complex z) z)
; (put-coercion 'scheme-number 'scheme-number
;               scheme-number->scheme-number)
; (put-coercion 'complex 'complex complex->complex)

;;; Assume we define:
(define (exp x y) (apply-generic 'exp x y))

;;; (a) What if exp is called for two complex numbers?
;;;   We get an infinite loop because we always find a coercion procedure, but
;;;   never change the type of the argument.
;;; Test code:
(define a (make-complex-from-real-imag 1 1))
(define b (make-complex-from-real-imag 1 2))
; (exp a b) ; Never completes!!

;;; (b) Is Louis correct that something had to be done about coercion with
;;; arguments of the same type, or does apply-generic work correctly as is?
;;;   If no operation is found, i.e. (exp a b) where each is complex,
;;;   apply-generic will just give an error "No method for these types".

;;; (c) modify apply-generic so that it will not try coercion if two types are
;;; the same
(define (apply-generic op . args)
  (define (no-method tt)
    (error "No method for these types"
           (list op tt)))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            ;; Test if types are the same:
            (if (equal? type1 type2)
              (no-method type-tags)
              (let ((t1->t2 (get-coercion type1 type2))
                    (t2->t1 (get-coercion type2 type1)))
                (cond (t1->t2
                        (apply-generic op (t1->t2 a1) a2))
                      (t2->t1
                        (apply-generic op a1 (t2->t1 a2)))
                      (else
                        (no-method type-tags))))))
          (no-method type-tags))))))

;;; Test code:
(define c (make-scheme-number 3))
(add a c)
;;==============================================================================
;;==============================================================================
