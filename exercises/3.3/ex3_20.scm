;;==============================================================================
;;     File: ex3_20.scm
;;  Created: 01/15/2017, 16:12
;;   Author: Bernie Roesler
;;
;;  Description: cons car cdr as assignment
;;
;;==============================================================================

;;; Define procedures in terms of assignments
(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)
(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)
(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

;;; Draw environment diagrams to represent this evaluation:
(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)
(car x) ; Value: 17

;;==============================================================================
;;==============================================================================
