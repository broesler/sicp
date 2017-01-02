;;==============================================================================
;;     File: rationals.scm
;;  Created: 10/29/2016, 01:44
;;   Author: Bernie Roesler
;;
;;  Description: Rational number computation
;;
;;==============================================================================

;;; Make-rat data structure
; (define (make-rat n d) (cons n d)) ; old definition
; Redefine to reduce during construction
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

;;; Rational numbers procedure
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;;; Pretty-printing
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; Test code:
(define (test-rats)
  (define one-half  (make-rat 1 2))
  (define one-third (make-rat 1 3))
  (print-rat one-half)
  (print-rat (add-rat one-half one-third))  ; Value: 5/6
  (print-rat (mul-rat one-half one-third))  ; Value: 1/6
  (print-rat (add-rat one-third one-third))) ; Value: 6/9 ==> 2/3 with gcd!
; (test-rats)
;;==============================================================================
;;==============================================================================
