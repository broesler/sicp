;;==============================================================================
;;     File: ex2_93-7.scm
;;  Created: 01/06/2017, 15:42
;;   Author: Bernie Roesler
;;
;;  Description: Rational Functions
;;
;;==============================================================================
(load "polynomials.scm")

;-------------------------------------------------------------------------------
;        Ex 2.93
;-------------------------------------------------------------------------------
;;
;;  x^3 + 1     x^3 + 1     2x^3 + 2       2x^5 + 2x^3 + 2x^2 + 2
;; --------- + --------- = ----------- = ------------------------- not reduced!
;;  x^2 + 1     x^2 + 1      x^2 + 1           x^4 + 2x^2 + 1
;;
(define p1 (make-polynomial 'x '((2 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 1))))
(define rf (make-rational p2 p1))

;;; Test equ?
(equ? p1 p1) ; Value: #t
(equ? p1 p2) ; Value: #f

(printval (add rf rf))
;; Prior to creating reduce-poly:
; Value:  (rational (polynomial x (5 2) (3 2) (2 2) (0 2))
;                    polynomial x (4 1) (2 2) (0 1))

;; With reduce-poly:
; Value: (rational (polynomial x (3 1) (0 1)) polynomial x (2 1) (0 1))
; (x^3 + 1) / (x^2 + 1) --> wrong :( should be as above

;-------------------------------------------------------------------------------
;        Ex 2.94
;-------------------------------------------------------------------------------
;; (gcd (x^4 - x^3 - 2x^2 + 2x) (x^3 - x)) = -x^2 + x
(define p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
(define p2 (make-polynomial 'x '((3 1) (1 -1))))
(printval (greatest-common-divisor p1 p2)) ; Value: (polynomial x (2 -1) (1 1))

;; Test GCD on regular numbers
(printval (greatest-common-divisor 220 40)) ; Value: 20

;-------------------------------------------------------------------------------
;        Ex 2.95
;-------------------------------------------------------------------------------
(define P1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
(define P2 (make-polynomial 'x '((2 11) (0 7))))
(define P3 (make-polynomial 'x '((2 13) (0 5))))
(define Q1 (mul P1 P2))
(define Q2 (mul P1 P3))
(greatest-common-divisor Q1 Q2)
;;; Using remainder-terms:
; Value: (polynomial x (2 36/13) (1 -72/13) (0 36/13))

;-------------------------------------------------------------------------------
;        Ex 2.96
;-------------------------------------------------------------------------------
;;; (a) Implement pseudodivision
(greatest-common-divisor Q1 Q2)
;;; Using pseudoremainder-terms:
; Value: (polynomial x (2 36) (1 -72) (0 36))

;;; (b) modify gcd-terms so it removes common factors from the coefficients of
;;; the answer by dividing all coefficients by their (integer) GCD
(printval (greatest-common-divisor Q1 Q2))
; Value: (polynomial x (2 1) (1 -2) (0 1))

;-------------------------------------------------------------------------------
;        Ex 2.97
;-------------------------------------------------------------------------------
;;; (a) create reduce-terms
;;; 1. Get gcd of numerator and denominator
;;; 2. Multiply n and d by integerizing factor
;;; 3. Divide n/f and d/f by gcd ==> n and d have integer coeffs!
;;; 4. Reduce terms of n and d to get coeffs smaller

;;; (b) Create reduce as generic operation for integers or polynomials
(define p1 (make-polynomial 'x '((1 1)(0 1))))
(define p2 (make-polynomial 'x '((3 1)(0 -1))))
(define p3 (make-polynomial 'x '((1 1))))
(define p4 (make-polynomial 'x '((2 1)(0 -1))))

(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))

(printval (add rf1 rf2))
; Value: (rational (polynomial x (3 1) (2 2) (1 3) (0 1)) 
;                   polynomial x (4 -1) (3 -1) (1 1) (0 1))

;;   x^3 + 2x^2 + 3x + 1
;; ----------------------
;;  -x^4 - x^3 +  x + 1
;;==============================================================================
;;==============================================================================
