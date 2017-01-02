;;==============================================================================
;;     File: ex2_6.scm
;;  Created: 11/09/2016, 17:51
;;   Author: Bernie Roesler
;;
;;  Description: Church numerals 
;;
;;==============================================================================

;;; Church numeral representation:
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;;; Define "one" and "two" directly (not in terms of zero and add-1)
;;; Apply function one or two times
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

;;; Define "+" directly (not in terms of add-1)
; "Apply function a+b times 
;   == apply function a times to function that is applied b times
; a and b are (λ(f) (λ(x) (f ... x)))
(define (plus a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

;; i.e. (plus one two) --> (lambda (f) (lambda (x) (f (f (f x)))))

;; Test code:
(newline)
(display ((zero square) 2)) ; Value = 2
(newline)
(display ((one square) 2))  ; Value = 2^2 = 4
(newline)
(display ((two square) 2))  ; Value = 2^2^2 = 2^4 = 16
(newline)
(display (((add-1 one) square) 2)) ; Value = 2^2^2 = 2^4 = 16
(newline)
(display (((plus one two) square) 2)) ; Value = 2^2^2^2 = 2^8 = 256
;;==============================================================================
;;==============================================================================
