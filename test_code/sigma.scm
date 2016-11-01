;;==============================================================================
;;     File: sigma.scm
;;  Created: 10/28/2016, 14:40
;;   Author: Bernie Roesler
;;
;;  Description: sigma notation
;;==============================================================================

; sum <term> from i = <a> to <b>, where <a> is updated by <next>
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a) 
       (sum term (next a) next b))))

; sum integers
(define (sum-int a b)
  (define (identity a) a)
  (sum identity a 1+ b))

; sum squares
(define (sum-sqr a b)
  (sum square a 1+ b))

; pi sum
(define (pi-sum a b)
  (sum (lambda (i) (/ 1.0 (* i (+ i 2.0))))
       a
       (lambda (i) (+ i 4.0))
       b))
;;==============================================================================
;;==============================================================================
