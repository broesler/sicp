;;==============================================================================
;;     File: ps5_exercises.scm
;;  Created: 01/05/2017, 14:03
;;   Author: Bernie Roesler
;;
;;  Description: Solutions to PS5 Generic Arithmetic exercises
;;
;;==============================================================================
(load "put-get.scm")
(load "types.scm")
(load "ps5-code.scm")

;------------------------------------------------------------------------------- 
;        Exercise 5.1
;-------------------------------------------------------------------------------
;;; (A) "install" =number in number package:
;; (RepNum, RepNum) -> Bool
(define (=number x y)
  (= x y))

;;; (B) install equ? as generic operator
;; equ? : (GN, GN) -> Bool
(define (equ? x y) (apply-generic 'equ? x y))

;; put into table
(put 'equ? '(number number) =number)

;; Test code:
(newline)
(display ";;; Exercise 5.1")
(printval (equ? (create-number 3) (create-number 3))) ; Value: #t
(printval (equ? (create-number 1) (create-number 3))) ; Value: #f

;------------------------------------------------------------------------------- 
;        Exercise 5.2
;-------------------------------------------------------------------------------
(define r5/13 (create-rational (create-number 5) (create-number 13)))
(define r2 (create-rational (create-number 2) (create-number 1)))
(define rsq (square (add r5/13 r2)))
(newline)
(display ";;; Exercise 5.2")
(printval rsq)
; Value: (rational (number . 961) number . 169)

;------------------------------------------------------------------------------- 
;        Exercise 5.3
;-------------------------------------------------------------------------------
;;; (A) define equ-rat?
;;; (RepRat,RepRat) -> Bool
(define (equ-rat? x y)
  (and (equ? (numer x) (numer y))
       (equ? (denom x) (denom y))))

;;; (B) install as generic operator
(put 'equ? '(rational rational) equ-rat?)

;;; Test code:
(newline)
(display ";;; Exercise 5.3")
(printval (equ? (create-number 3) (create-number 3))) ; Value: #t
(printval (equ? (create-number 1) (create-number 3))) ; Value: #f
(printval (equ? r5/13 r5/13)) ; Value: #t
(printval (equ? r5/13 r2)) ; Value: #f

;------------------------------------------------------------------------------- 
;        Exercise 4
;-------------------------------------------------------------------------------
(define n2 (create-number 2))
;;; Before adding the new methods, the following returns:
;;;     (add n2 r5/13) 
;;; ;No method for the given types -- APPLY-GENERIC (add (number rational))

;;; (A) define procedure
;;;     repnum->reprat : RepNum -> RepRat
(define (repnum->reprat n)
  (create-rational n (create-number 1)))

;;; Test code:
(repnum->reprat n2) ;Value: (rational (number . 2) number . 1) 

;;; Define generic methods for both (num rat) and (rat num)
;;; When add is called with a number and a rational, apply-generic will apply
;;; a procedure that converts the number to a rational, then calls the generic
;;; add again, which will apply a procedure that adds two rationals. 

(define (tag-args-NR method)
  (lambda (n r)
    ((RRmethod->NRmethod method) (attach-tag 'number n) 
                                 (attach-tag 'rational r))))

(define (tag-args-RN method)
  (lambda (r n)
    ((RRmethod->RNmethod method) (attach-tag 'rational r)
                                 (attach-tag 'number n))))

;;; apply-generic strips the tags off of our numbers BEFORE passing them to the
;;; conversion procedure, so we need to artificially add them back in.
(put 'add '(number rational) (tag-args-NR add))
(put 'sub '(number rational) (tag-args-NR sub))
(put 'mul '(number rational) (tag-args-NR mul))
(put 'div '(number rational) (tag-args-NR div))

;;;  ((RepRat,RepRat) --> T) --> ((RepRat,RepNum) --> T)
(define (RRmethod->RNmethod method)
  (lambda (rat num) 
    (method rat (repnum->reprat num))))

(put 'add '(rational number) (tag-args-RN add))
(put 'sub '(rational number) (tag-args-RN sub))
(put 'mul '(rational number) (tag-args-RN mul))
(put 'div '(rational number) (tag-args-RN div))

;; Equality
(put 'equ? '(number rational) (tag-args-NR equ?))
(put 'equ? '(rational number) (tag-args-RN equ?))

;;; (B) Install and test new methods
(newline)
(display ";;; Exercise 5.4")
(printval (equ? (sub (add n2 r5/13) r5/13) n2)) ; Value: #f
;; does not reduce inner fraction to lowest terms!!!

;------------------------------------------------------------------------------- 
;        Exercise 5.5
;-------------------------------------------------------------------------------
;;==============================================================================
;;==============================================================================
