;;==============================================================================
;;     File: rationals.scm
;;  Created: 10/29/2016, 01:44
;;   Author: Bernie Roesler
;;
;;  Description: Rational number computation
;;
;;==============================================================================

; Rational number data structure {
; Constructor
(define (make-rat N d) 
  (cons n d))
; Better way:
; (define (make-rat n d)
;   (let ((g (gcd n d)))
;     (cons (/ n g)
;           (/ d g))))

; Selectors
(define (numer x) (car x))
(define (denom x) (cdr x))
; }

; Sum 2 rational numbers, and return a rational number
(define (+rat x y)
  (make-rat
    (+ (* (numer x) (denom y))
       (* (numer y) (denom x)))
    (* (denom x) (denom y))))
       
; Multiply 2 rational numbers
(define (*rat x y)
  (make-rat
    (* (numer x) (numer y))
    (* (denom x) (denom y))))

; Test code: 
;   1/2 + 1/4 = 3/4?
; (define a (make-rat 1 2))
; (define b (make-rat 1 4))
; (define ans (+rat a b))
; (numer ans) => 6
; (denom ans) => 8
;   ==> need to update make-rat to get GCD!
;;==============================================================================
;;==============================================================================
