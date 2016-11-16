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
;;; Iterative version (UGLY):
(define (solve-ax+by=1 a b)
  (define (solve-iter old-s old-t old-r s t r)
    (cond ((= r 0) (cons old-s old-t)) 
          (else 
            (let ((q (quotient old-r r)))
              (let ((old-s s)
                  (old-t t)
                  (old-r r)
                  (s (- old-s (* q s)))
                  (t (- old-t (* q t)))
                  (r (- old-r (* q r))))
            (solve-iter old-s old-t old-r s t r))))))
  (solve-iter 1 0 a 0 1 b))

; function extended_gcd(a, b)
;     s := 0;    old_s := 1
;     t := 1;    old_t := 0
;     r := b;    old_r := a
;     while r ≠ 0
;         quotient := old_r div r
;         (old_r, r) := (r, old_r - quotient * r)
;         (old_s, s) := (s, old_s - quotient * s)
;         (old_t, t) := (t, old_t - quotient * t)
;     output "Bézout coefficients:", (old_s, old_t)
;     output "greatest common divisor:", old_r
;     output "quotients by the gcd:", (t, s)

;;; Returns a pair (x . y)
(define (solve-ax+by=1 a b)
  (if (= b 0)
    (cons 1 (- 1))
    (solve-ax+by=1 b (remainder a b))))

;;; Test code:
(define (test-solve-ax+by=1 a b)
  (let* ((ans (solve-ax+by=1 a b))
         (x (car ans))
         (y (cdr ans)))
    (= (gcd a b) (+ (* a x) (* b y)))))

(SHOULD-BE (test-solve-ax+by=1 384 256))

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
