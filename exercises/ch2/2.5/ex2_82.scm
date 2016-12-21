;;==============================================================================
;;     File: ex2_82.scm
;;  Created: 12/19/2016, 23:30
;;   Author: Bernie Roesler
;;
;;  Description: Generalize apply-generic to more than 2 arguments
;;
;;==============================================================================
;;; Try to coerce all arguments to type of first argument, then second, etc.
;;; until a procedure is found
(define (apply-generic op . args)
  ;; Error message
  (define no-method
    (error "No method for these types"
           (list op type-tags)))

  ;; Apply found procedure to coerced list of arguments
  (define (apply-coerced xargs)
    (if (null? xargs)
      (no-method)
      ;; coerce entire argument list, xargs is iterated through
      (let ((coerced-list (coerce-list args (type-tag (car xargs)))))
        (let ((proc (get op (map type-tag coerced-list))))
          (if proc
            (apply proc (map contents coerced-list))
            (apply-coerced (cdr xargs)))))))

  ;; Coerce a list to all the same type
  (define (coerce-list lst type)
    (if (null? lst)
      '()
      (let ((t1->t2 (get-coercion (type-tag (car lst)) type)))
        (if t1->t2
          (cons (t1->t2 (car lst)) (coerce-list (cdr lst) type))
          (cons (car lst) (coerce-list (cdr lst) type))))))

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
(add a b c)

;;==============================================================================
;;==============================================================================
