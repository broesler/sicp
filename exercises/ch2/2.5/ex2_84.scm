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
  ;; compare 2 elements at a time:
  ;;    (a b c d) => ((a b) c d) => (a (b c) d) => (a b (c d))
  ;; raise lower element up to higher element, or nothing if they're the same 
  ;; cycle through the list until all elements are the same
  ;; Try to coerce a list to all the same level of the tower
  (define (raise-list elems)
    (if (all-same-type? elems)
      elems
      (let ((a (car elems))
            (b (cadr elems)))
        (cond ((higher? a b) 
               (raise-list 
                 (cons a (cons (raise b a) (cddr elems)))))
              ((higher? b a) 
               (raise-list 
                 (cons (raise a b) (cons b (cddr elems)))))
              (else (raise-list elems))))))

  ;; Apply operation in pairs, and cons together since our package's arithmetic
  ;; procedures were specified for maximum of 2 arguments:
  (define (apply-by-twos op args) 
    (if (null? (cdr args)) 
      (car args) 
      (let ((a1 (car args)) 
            (a2 (cadr args)) 
            (rest-args (cddr args))) 
        (apply-by-twos op (cons (apply-generic op a1 a2) rest-args))))) 

  ;; Main procedure
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (apply-generic op (raise-list args))))))

;; Check if all elements are the same type
(define (all-same-type? elems)
  (define (iter lst)
    (if (null? (cdr lst))
      true
      (if (equal? (type-tag (car lst)) 
                  (type-tag (cadr lst)))
        (iter (cdr lst))
        false)))
  (iter elems))

; ;; Check if element a is higher in tower than b
; (define (higher? a b)

;;; Test code:
(define a (make-complex-from-real-imag 1 1))
(define b (make-rational 3 4))
(define c (make-scheme-number 7))
(printval (add a b))
(printval (add a c))
(printval (add b c))

;;==============================================================================
;;==============================================================================
