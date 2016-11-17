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
;;; Returns a pair (x . y)
(define (solve-ax+by=1 a b)
  (define (solve-iter a b x y)
    (newline)
    (display "; a = ") (display a)
    (display "; b = ") (display b)
    (display "; x = ") (display x)
    (display "; y = ") (display y)
    (if (= b 0) 
      (cons x y)
      (solve-iter b
                  (remainder a b)
                  y
                  (- x (* (quotient a b) y)))))
  (solve-iter a b 1 0))

; Euclid's algorithm for gcd
; ; use eqn gcd(a,b) = gcd(b,r), where r = a % b
; (define (gcd a b)
;   (if (= b 0)
;       a
;       (gcd b (remainder a b))))

;;; Test code:
;;; (a,b) --> (x,y)
;;; (27,4) --> (-8,54)
(let* ((a 27) ; choose two numbers s.t. (gcd a b) == 1
       (b  4)
       (ans (solve-ax+by=1 a b))
       (x (car ans))
       (y (cdr ans)))
  (printval ans)
  (printval (+ (* a x) (* b y))))

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
