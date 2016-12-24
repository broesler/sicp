;;==============================================================================
;;     File: ex2_84.scm
;;  Created: 12/23/2016, 18:39
;;   Author: Bernie Roesler
;;
;;  Description: Redefine apply-generic for use with (raise)
;;
;;==============================================================================
(load "generic_arithmetic.scm")

;;; (a) apply-generic with arbitrary number of arguments
(define (apply-generic op . args)
  ;; Apply found procedure to coerced list of arguments
  (define (apply-coerced elems)
    (if (null? elems)
      (error "No method for these types"
             (list op (map type-tag args)))
      ;; coerce entire argument list, elems is iterated through
      (let ((coerced-list (coerce-list args (type-tag (car elems)))))
        (let ((type-tags (map type-tag coerced-list)))
          (let ((proc (get op type-tags)))
            (if proc
              (apply proc (map contents coerced-list))
              (apply-coerced (cdr elems))))))))

  ;; Try to raise a list to all the same level of the tower
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
(define b (make-rational 3 4))
(define c (make-scheme-number 7))
(printval (add a b))
(printval (add a c))
(printval (add b c))

;;==============================================================================
;;==============================================================================
