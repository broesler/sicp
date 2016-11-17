;;==============================================================================
;;     File: ps3_rsa.scm
;;  Created: 11/14/2016, 20:17
;;   Author: Bernie Roesler
;;
;;  Description: RSA Encryption 
;;
;;==============================================================================
(load "rsa.scm")

;;; Test function
(define (SHOULD-BE x?)
  (newline)
  (if x?
    (display "Passed test!")
    (display "Failed test!")))

;------------------------------------------------------------------------------- 
;        Test code
;-------------------------------------------------------------------------------
(define test-public-key1 (key-pair-public test-key-pair1))
(define result1 (rsa-encrypt "This is a test message." test-public-key1))
(SHOULD-BE (equal? 
             result1 
             '(209185193 793765302 124842465 169313344 117194397 237972864)))

;------------------------------------------------------------------------------- 
;        Define solve-ax+by=1
;-------------------------------------------------------------------------------
;;; Extended Euclidean algorithm to solve a*x + b*y = gcd(a,b)
;;; Tracks x,y directly
(define (solve-ax+by=1 a b)
  (define (solve-iter a b q x y)
    (newline)
    (display "; a = ") (display a) ; == old b
    (display "; b = ") (display b) ; == old (remainder a b)
    (display "; q = ") (display q) ; == old (quotient a b)
    (display "; x = ") (display x) ; == old x == y
    (display "; y = ") (display y) ; == old y == x - qy
    (if (= b 0) 
      (cons x y) ; Return the pair (x.y)
      (solve-iter b
                  (remainder a b)
                  (quotient a b)
                  y ; "x"
                  (- x (* (quotient a b) y))))) ; "y"
  (solve-iter a b 0 1 0))

;;; Test code:
;;; (  a,  b) --> (  x,  y)
;;; ( 27,  4) --> ( -1,  7)
;;; ( 56,  9) --> ( -4, 25)
;;; (207, 40) --> (-17, 88)
(define (test-solve a b) ; choose two numbers s.t. (gcd a b) == 1
  (let* ((ans (solve-ax+by=1 a b))
         (x (car ans))
         (y (cdr ans))
         (g (gcd a b))
         (sum (+ (* a x) (* b y))))
    (printval ans)
    (printval g)
    (printval sum)
    ; (printval (= g sum))
  ))

(test-solve 27 4)
(test-solve 56 9)
(test-solve 207 40)

;;; NOTE: x-value is ALWAYS correct. y-value is low by 3, 16, 48 
;------------------------------------------------------------------------------- 
;       Exercises 
;-------------------------------------------------------------------------------
;;; Ex 1: no difference!
;;; Ex 2: paper
;;; Ex 3: no, because then you have to un-hash the message
;;; Ex 4: Not secure because you then only have to decrypt (in this case)
;;;   a 4-digit number!!
;;;
;;; (define (RSA-encrypt intlist key1)
;;;   (map (lambda (int) (RSA-transform int key1)) 
;;;        intlist))
;;;
;;; Analogous decrypt:
;;; (define (RSA-decrypt intlist key2)
;;;   (map (lambda (int) (RSA-transform int key2)) 
;;;        intlist))
;;==============================================================================
;;==============================================================================
