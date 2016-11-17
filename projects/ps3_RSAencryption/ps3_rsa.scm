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
    (display "; Passed test!")
    (display "; Failed test!")))

;------------------------------------------------------------------------------- 
;       2. Exercises 
;-------------------------------------------------------------------------------
;;; Ex 1: no difference!
;;; Ex 2: paper
;;; Ex 3: no, because then you have to un-compress the message
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

;------------------------------------------------------------------------------- 
;        4. Assignment 
;-------------------------------------------------------------------------------
;;; Test code
(define test-public-key1 (key-pair-public test-key-pair1))
(define result1 (rsa-encrypt "This is a test message." test-public-key1))
(SHOULD-BE (equal? 
             result1 
             '(209185193 793765302 124842465 169313344 117194397 237972864)))

;------------------------------------------------------------------------------- 
;        Ex 1. RSA-unconvert-list
;-------------------------------------------------------------------------------
;;; For reference:
; (define (RSA-convert-list intlist key)
;   (let ((n (key-modulus key)))
;     (define (convert l sum)
;       (if (null? l)
;         '()
;         (let ((x (RSA-transform (modulo (- (car l) sum) n)
;                                 key)))
;           (cons x (convert (cdr l) x)))))
;     (convert intlist 0)))

(define (RSA-unconvert-list intlist key)
  (let ((n (key-modulus key)))
    (define (unconvert l sum)
      (if (null? l)
        '()
        (let ((x (modulo (+ (RSA-transform (car l) key) sum) n)))
          (cons x (unconvert (cdr l) (car l))))))
    (unconvert intlist 0)))

;;; Test code:
(define test-private-key1 (key-pair-private test-key-pair1))
(define unconv-result1 (RSA-unconvert-list result1 test-private-key1))
(SHOULD-BE (equal? 
             unconv-result1
             '(242906196 69006496 213717089 229128819 205322725 67875559)))

;;; Decrypt result! 
;;;   same as: (intlist->string unconv-result1)
(RSA-decrypt result1 test-private-key1)

;;; Second test key
(define test-public-key2 (key-pair-public test-key-pair2))
(define test-private-key2 (key-pair-private test-key-pair2))
(define result2 (RSA-encrypt "Hello, World!" test-public-key2))
(RSA-decrypt result2 test-private-key2)

; ;------------------------------------------------------------------------------- 
; ;        Ex. 4 Define solve-ax+by=1
; ;-------------------------------------------------------------------------------
; ;;; Extended Euclidean algorithm to solve a*x + b*y = gcd(a,b)
; ;;; Tracks x,y directly
; (define (solve-ax+by=1 a b)
;   ;; Euclid's algorithm
;   (if (= b 0) 
;     (cons 1 0) ; (x.y) base case
;     (let* ((q (quotient a b))
;            (r (remainder a b))
;            (xy (solve-ax+by=1 b r)) ; recursively solve til gcd found
;            (x (car xy))
;            (y (cdr xy)))
;       (cons y (- x (* q y)))))) ; output (x.y) as derived
;
; ;;; Test function:
; (define (test-solve a b) ; choose two numbers s.t. (gcd a b) == 1
;   (let* ((ans (solve-ax+by=1 a b))
;          (x (car ans))
;          (y (cdr ans))
;          (g (gcd a b))
;          (sum (+ (* a x) (* b y))))
;     ; (printval ans)
;     (= g sum)))
;
; ;;; Test cases:
; ;;; (  a,  b) --> (  x,  y)
; ;;; ( 27,  4) --> ( -1,  7)
; ;;; ( 56,  9) --> ( -4, 25)
; ;;; (207, 40) --> (-17, 88)
; ; (SHOULD-BE (test-solve 27 4))
; ; (SHOULD-BE (test-solve 56 9))
; ; (SHOULD-BE (test-solve 207 40))
; (SHOULD-BE (test-solve 233987973 41111687)) ; Exercise check
; ;;==============================================================================
; ;;==============================================================================
