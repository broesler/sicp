;;==============================================================================
;;     File: ex2_82.scm
;;  Created: 12/19/2016, 23:30
;;   Author: Bernie Roesler
;;
;;  Description: Generalize apply-generic to more than 2 arguments
;;
;;==============================================================================
(load "generic_arithmetic.scm")

;;; (a) apply-generic with arbitrary number of arguments
(define (apply-generic op . args)
  ;; Apply found procedure to coerced list of arguments
  (define (apply-coerced xargs)
    (if (null? xargs)
      (error "No method for these types"
             (list op (map type-tag args)))
      ;; coerce entire argument list, xargs is iterated through
      (let ((coerced-list (coerce-list args (type-tag (car xargs)))))
        (let ((type-tags (map type-tag coerced-list)))
          (let ((proc (get op type-tags)))
            (if proc
              (apply proc (map contents coerced-list))
              (apply-coerced (cdr xargs))))))))

  ;; Try to coerce a list to all the same type
  ;; if no coercion is found, just leave the type as-is
  (define (coerce-list lst type)
    (map (lambda (x) 
           (let ((t1->t2 (get-coercion (type-tag x) type)))
             (if t1->t2 (t1->t2 x) x))) 
         lst))

  ;; Main procedure
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (apply-coerced args)))))

;;; Test code:
(define a (make-complex-from-real-imag 1 1))
(define b (make-complex-from-real-imag 1 2))
(define c (make-scheme-number 3))
(printval (add a b))
(printval (add a c))
; (printval (add a b c))

;;; (b) Give an example of a situation where this strategy (and likewise the
;;; two-argument version given above) is not sufficiently general. (Hint:
;;; Consider the case where there are some suitable mixed-type operations
;;; present in the table that will not be tried.) 

;;==============================================================================
;;==============================================================================
