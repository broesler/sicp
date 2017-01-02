;;==============================================================================
;;     File: ex2_75.scm
;;  Created: 11/27/2016, 19:55
;;   Author: Bernie Roesler
;;
;;  Description: make-from-mag-ang in message passing style 
;;
;;==============================================================================
;;; Given code:
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

;;; Re-define generic procedure:
(define (apply-generic op arg) (arg op))

;;; Write corresponding procedure:
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part) 
           (* r (cos a)))
          ((eq? op 'imag-part) 
           (* r (sin a)))
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;;; Re-run tests
(load "complex_test.scm")
;;; Show how message-passing works
(pp real-part)
(pp z1)
;;==============================================================================
;;==============================================================================
