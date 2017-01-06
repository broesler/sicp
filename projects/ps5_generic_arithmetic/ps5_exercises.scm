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
;; (RepNum, RepNum) --> Bool
(define (=number x y)
  (= x y))

;;; (B) install equ? as generic operator
;; equ? : (GN, GN) --> Bool
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
;;; (RepRat, RepRat) --> Bool
(define (equ-rat? x y)
  (and (equ? (numer x) (numer y))
       (equ? (denom x) (denom y))))

;;; (RepRat, RepRat) --> Bool
(define (equ-rational? x y) (equ-rat? x y))

;;; (B) install as generic operator
(put 'equ? '(rational rational) equ-rational?)

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
;;; Before adding the new methods, the following returns:
;;;     (add n2 r5/13) 
;;; ;No method for the given types -- APPLY-GENERIC (add (number rational))

;;; (A) define procedure
;;;     repnum->reprat : RepNum --> RepRat
(define (repnum->reprat n)
  (make-rat (create-number n) (create-number 1)))

;;; Define generic methods for both (num rat) and (rat num)
;;; add, sub, mul, div : (GN, GN) --> GN
;;; put procedure that takes (RepNum, RepRat) --> ({rational} X RepRat)
(put 'add '(number rational) (RRmethod->NRmethod +rational))
(put 'sub '(number rational) (RRmethod->NRmethod -rational))
(put 'mul '(number rational) (RRmethod->NRmethod *rational))
(put 'div '(number rational) (RRmethod->NRmethod /rational))

;;; Same as above, but switch the order of the arguments
;;;  ((RepRat, RepRat) --> T) --> ((RepRat, RepNum) --> T)
(define (RRmethod->RNmethod method)
  (lambda (rat num) 
    (method rat (repnum->reprat num))))

;;; put procedure that takes (RepRat, RepNum) --> ({rational} X RepRat)
(put 'add '(rational number) (RRmethod->RNmethod +rational))
(put 'sub '(rational number) (RRmethod->RNmethod -rational))
(put 'mul '(rational number) (RRmethod->RNmethod *rational))
(put 'div '(rational number) (RRmethod->RNmethod /rational))

;; Equality
;;; equ? : GN --> Bool
;;; put procedure that takes (RepRat, RepNum) --> Bool
(put 'equ? '(number rational) (RRmethod->NRmethod equ-rational?))
(put 'equ? '(rational number) (RRmethod->RNmethod equ-rational?))

;;; (B) Install and test new methods
(newline)
(display ";;; Exercise 5.4")
(printval (equ? (sub (add n2 r5/13) r5/13) n2)) ; Value: #f
;; does not reduce inner fraction to lowest terms!!!

;------------------------------------------------------------------------------- 
;        Exercise 5.5
;-------------------------------------------------------------------------------
;;; (A) What type it map-terms?
;;; map-terms : ((RepTerm->RepTerm), RepTerms) --> RepTerms
(define (map-terms proc tlist)
  (if (empty-termlist? tlist)
    (the-empty-termlist)
    (adjoin-term (proc (first-term tlist))
                 (map-terms proc (rest-terms tlist)))))

;;; (B) 
;;; create-numerical-polynomial : (Variable, List(Sch-Num)) --> RepPoly
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
;;; Dispatch table
;;; Op Type->   RepNum    RepRat    RepPoly
;;; add           ✓         ✓         ✓
;;; sub           ✓         ✓         ✗
;;; mul           ✓         ✓         ✓
;;; div           ✓         ✓         ✗
;;; negate        ✓         ✓         ✗
;;; =zero?        ✓         ✓         ✗
;;; equ?          ✓         ✓         ✗

;;; (A) use map-terms to write negate-terms, then negate-poly
;; negate-terms : RepTerms --> RepTerms
(define (negate-terms tlist)
  (map-terms (lambda (x) 
               (make-term (order x) 
                          (negate (coeff x)))) 
             tlist))

;; negate-poly : RepPoly --> RepPoly
(define (negate-poly poly)
  (make-poly (variable poly) (negate-terms (term-list poly))))

;; negate-polynomial : RepPoly --> ({polynomial} X RepPoly)
(define (negate-polynomial poly)
  (make-polynomial (negate-poly poly)))

;;; (B)
;; -poly : (RepPoly, RepPoly) --> RepPoly
(define (-poly p1 p2)
  (+poly p1 (negate-poly p2)))

;; -polynomial : (RepPoly, RepPoly) --> ({polynomial} X RepPoly)
(define (-polynomial p1 p2)
  (make-polynomial (-poly p1 p2)))

;; equ-poly? : (RepPoly, RepPoly) --> Bool
(define (equ-poly? p1 p2)
  (=zero-poly? (-poly p1 p2)))

;; equ-polynomial? : (RepPoly, RepPoly) --> Bool
(define (equ-polynomial? p1 p2)
  (equ-poly? p1 p2))

;;; (C) install procedures
(put 'negate '(polynomial) negate-polynomial)
(put 'sub '(polynomial polynomial) -polynomial)
(put 'equ? '(polynomial polynomial) equ-polynomial?)

;;; Test code:
(newline)
(display ";;; Exercise 5.7")
(printval (negate p1))
; Value: (polynomial x (3 (number . -1)) (2 (number . -5)) (0 (number . 2)))
(printval (equ? p2 p2)) ; Value: #t
(printval (=zero? (sub p3 p3))) ; Value: #t

;------------------------------------------------------------------------------- 
;        Exercise 5.8
;-------------------------------------------------------------------------------
;;; (A) repnum->reppoly : (Variable, RepNum) --> RepPoly
(define (repnum->reppoly var n)
  (make-poly var (adjoin-term (make-term 0 (create-number n))
                              (the-empty-termlist))))

;;; Test code:
(newline)
(display ";;; Exercise 5.8")
(printval (repnum->reppoly 'x 3)) 
; Value: (x (0 (number . 3)))

;;; Same as above, but switch the order of the arguments
;;;  ((RepPoly, RepPoly) --> T) --> ((RepPoly, RepNum) --> T)
(define (PPmethod->NPmethod method)
  (lambda (num poly) 
    (method (repnum->reppoly (variable poly) num) poly)))

;;; Define methods for generic add, sub, mul, equ? at types (number polynomial),
;;; and (polynomial number), and div at (polynomial number)
;;; put procedure that takes (RepNum, RepPoly) --> ({polynomial} X RepPoly)
(put 'add '(number polynomial) (PPmethod->NPmethod +polynomial))
(put 'sub '(number polynomial) (PPmethod->NPmethod -polynomial))
(put 'mul '(number polynomial) (PPmethod->NPmethod *polynomial))
;; NOT ALLOWED: (put 'div '(number polynomial) (PPmethod->NPmethod /polynomial))

;;; Same as above, but switch the order of the arguments
;;;  ((RepPoly, RepPoly) --> T) --> ((RepPoly, RepNum) --> T)
(define (PPmethod->PNmethod method)
  (lambda (poly num) 
    (method poly (repnum->reppoly (variable poly) num))))

;;; put procedure that takes (RepPoly, RepNum) --> ({polynomial} X RepPoly)
(put 'add '(polynomial number) (PPmethod->PNmethod +polynomial))
(put 'sub '(polynomial number) (PPmethod->PNmethod -polynomial))
(put 'mul '(polynomial number) (PPmethod->PNmethod *polynomial))
; (put 'div '(polynomial number) (PPmethod->PNmethod /polynomial))

;; TODO: create /polynomial for (polynomial number) ONLY, that divides every
;; term by the number... or that multiplies every term by 1/n? Then we need to
;; create a rational.

;;; (B) Test code:
(printval (equ? (sub (add p1 p3) p1) p3)) ; Value: #t
(newline)
(display "; (square p2-mixed) = ") (newline)
(pp (square p2-mixed))

;;; (C) Why can't we coerce p --> p/1 always? How about r --> (x (0 r))?
;;; We could have a rational polynomial (i.e. rational function)

;------------------------------------------------------------------------------- 
;        Exercise 5.9
;-------------------------------------------------------------------------------
;;; (A) Define apply-terms
;;; (RepTerms, GN) --> GN
(define (apply-terms terms gn)
  (if (empty-termlist? terms)
    (create-number 0)
    (add (apply-term (first-term terms) gn)
         (apply-terms (rest-terms terms) gn))))

;;; (B) Test code:
(printval (apply-polynomial p1 (create-number 2))) ; Value: 26
(newline)
(display "; apply p2 to x+1 = ") (newline)
(pp (apply-polynomial p2 (create-numerical-polynomial 'x (list 1 1))))
(define x (create-numerical-polynomial 'x '(1 0)))
(equ? (apply-polynomial p1 x) p1) ; Value: #t

;;==============================================================================
;;==============================================================================
