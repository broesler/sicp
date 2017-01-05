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
;;; (A) What type it map-terms?
;;; map-terms : ((RepTerm->RepTerm), RepTerms) -> RepTerms
(define (map-terms proc tlist)
  (if (empty-termlist? tlist)
    (the-empty-termlist)
    (adjoin-term (proc (first-term tlist))
                 (map-terms proc (rest-terms tlist)))))

;;; (B) 
;;; create-numerical-polynomial : (Variable, List(Sch-Num)) -> RepPoly
(define (create-numerical-polynomial var coeffs)
  (create-polynomial var (map create-number coeffs)))

(define p1 (create-numerical-polynomial 'x '(1 5 0 -2)))

;;; (C) Evaluate map-terms
(newline)
(display ";;; Exercise 5.5")
(newline)
(pp (square p1))
; (polynomial x
;             (6 (number . 1))
;             (5 (number . 10))
;             (4 (number . 25))
;             (3 (number . -4))
;             (2 (number . -20))
;             (0 (number . 4)))

;------------------------------------------------------------------------------- 
;        Exercise 5.6
;-------------------------------------------------------------------------------
(define p2-mixed
  (create-polynomial 'z (list p1 (create-number 3) (create-number 5))))
; (square p2-mixed) ; Fails!

;; Define coeffs as polynomials instead!
(define p2
  (create-polynomial 
    'z 
    (list p1 
          (create-polynomial 'x (list (create-number 3)))
          (create-polynomial 'x (list (create-number 5))))))

;;; (A) Define the following: p = 3/y, q = (y^2+1)/y, r = 1/(y-1), s = 2
(define p (create-rational (create-numerical-polynomial 'y (list 3))
                           (create-numerical-polynomial 'y (list 1 0))))
(define q (create-rational (create-numerical-polynomial 'y (list 1 0 1))
                           (create-numerical-polynomial 'y (list 1 0))))
(define r (create-rational (create-numerical-polynomial 'y (list 1))
                           (create-numerical-polynomial 'y (list 1 -1))))
(define s (create-rational (create-numerical-polynomial 'y (list 2))
                           (create-numerical-polynomial 'y (list 1))))

(define p3 (create-polynomial 'x (list p 
                                       (create-numerical-polynomial 'y (list 0)) 
                                       q 
                                       r 
                                       s)))

;;; (B)
(newline)
(display ";;; Exercise 5.6") (newline)
(display "(square p2) = ") (newline)
(pp (square p2))
(display "(square p3) = ") (newline)
(pp (square p3))
(display "(square (square p2)) = ") (newline)
(pp (square (square p2)))

;------------------------------------------------------------------------------- 
;        Exercise 5.7
;-------------------------------------------------------------------------------
;;==============================================================================
;;==============================================================================
