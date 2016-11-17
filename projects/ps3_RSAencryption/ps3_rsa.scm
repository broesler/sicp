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
(printval (RSA-decrypt result1 test-private-key1))

;;; Second test key
(define test-public-key2 (key-pair-public test-key-pair2))
(define test-private-key2 (key-pair-private test-key-pair2))
(define result2 (RSA-encrypt "Hello, World!" test-public-key2))
(printval (RSA-decrypt result2 test-private-key2))

;------------------------------------------------------------------------------- 
;        Ex. 2 signatures
;-------------------------------------------------------------------------------
;;; (a) Encrypt and sign
(define make-signed-message cons)
(define signed-message car)
(define signed-signature cdr)

(define (encrypt-and-sign message private-key public-key)
  (let* ((encrypted (RSA-encrypt message public-key))
         (compressed (compress encrypted))
         (signature (RSA-transform compressed private-key)))
    (make-signed-message encrypted signature)))

;;; Test encryption
(define result2
     (encrypt-and-sign "Test message from user 1 to user 2"
                       test-private-key1
                       test-public-key2))

(SHOULD-BE (equal? (signed-message result2) 
                   '(499609777 242153055 12244841 376031918 242988502 
                     31156692 221535122 463709109 468341391)))
(SHOULD-BE (equal? (signed-signature result2) '15378444))

;;; (b) Authenticate and decrypt
(define (authenticate-and-decrypt s-message public-key private-key)
  (let* ((encrypted (signed-message s-message))
         (decrypted (RSA-decrypt encrypted private-key))
         (compressed (compress encrypted))
         (maybe-sig (RSA-transform (signed-signature s-message) public-key)))
    (cond ((= compressed maybe-sig) 
           decrypted)
          (else 
            (newline) 
            (display "Warning! Message failed authentication!")
            (newline)
            (display "Message: ")
            (display decrypted)
            (newline)
            (display "Signature is: ") 
            (display maybe-sig)
            #f))))

;;; Recover original message!
(printval (authenticate-and-decrypt result2 
                                    test-public-key1 
                                    test-private-key2))

;;; Demonstrate incorrect public key for authentication
(define result3
     (encrypt-and-sign "Hello user 2."
                       test-private-key1
                       test-public-key2))
(printval (authenticate-and-decrypt result3 
                                    test-public-key2  ; WRONG PUBLIC KEY!!
                                    test-private-key2))

;------------------------------------------------------------------------------- 
;        Ex. 3 Mystery message!
;-------------------------------------------------------------------------------
(define mystery-message (make-signed-message 
                          received-mystery-message 
                          received-mystery-signature))

;;; Decrypt and print message
(define test-person-list (list bill-clinton-public-key
                               al-gore-public-key
                               bob-dole-public-key
                               ross-perot-public-key
                               hillary-clinton-public-key
                               tipper-gore-public-key
                               chuck-vest-public-key
                               rupert-murdoch-public-key))

(printval (map (lambda (x) (authenticate-and-decrypt 
                   mystery-message 
                   x 
                   newt-gingrich-private-key)) 
     test-person-list))

;;; Map is #f for all but last key! Rupert Murdoch is the sender!

;------------------------------------------------------------------------------- 
;        Ex. 4 Define solve-ax+by=1
;-------------------------------------------------------------------------------
;;; Extended Euclidean algorithm to solve a*x + b*y = gcd(a,b)
;;; Tracks x,y directly
(define (solve-ax+by=1 a b)
  ;; Euclid's algorithm
  (if (= b 0) 
    (cons 1 0) ; (x.y) base case
    (let* ((q (quotient a b))
           (r (remainder a b))
           (xy (solve-ax+by=1 b r)) ; recursively solve til gcd found
           (x (car xy))
           (y (cdr xy)))
      (cons y (- x (* q y)))))) ; output (x.y) as derived

;;; Test function:
(define (test-solve a b) ; choose two numbers s.t. (gcd a b) == 1
  (let* ((ans (solve-ax+by=1 a b))
         (x (car ans))
         (y (cdr ans))
         (g (gcd a b))
         (sum (+ (* a x) (* b y))))
    ; (printval ans)
    (= g sum)))

;;; Test cases:
;;; (  a,  b) --> (  x,  y)
;;; ( 27,  4) --> ( -1,  7)
;;; ( 56,  9) --> ( -4, 25)
;;; (207, 40) --> (-17, 88)
; (SHOULD-BE (test-solve 27 4))
; (SHOULD-BE (test-solve 56 9))
; (SHOULD-BE (test-solve 207 40))
(SHOULD-BE (test-solve 233987973 41111687)) ; Exercise check
;;==============================================================================
;;==============================================================================
