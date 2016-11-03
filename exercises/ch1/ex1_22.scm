;;==============================================================================
;;     File: ex1_22.scm
;;  Created: 11/03/2016, 16:47
;;   Author: Bernie Roesler
;;
;;  Description: Primality testing with (runtime) 
;;
;;==============================================================================

;------------------------------------------------------------------------------- 
;        Primality test copied from Ch 1.2
;-------------------------------------------------------------------------------
; Find smallest divisor of n (> 1) by testing n for divisibility by successive
; integers starting with 2. Growth is Θ(sqrt(n))
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)     ; no need to go bigger than sqrt(n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

; n is prime iff it is its own smallest divisor (other than 1)
(define (prime? n)
  (= n (smallest-divisor n)))

;------------------------------------------------------------------------------- 
;       Timing/printing function
;-------------------------------------------------------------------------------
; time the call
(define (timed-prime-test n)
  (start-prime-test n (runtime)))

; check for primality
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      #f))

; print result only if prime
(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

; Search for first n-primes above start
(define (search-for-primes start n-primes)
  (cond ((= n-primes 0) 
         "DONE")
        ((even? start) 
         (search-for-primes (+ start 1) n-primes))
        (else 
          (if (timed-prime-test start)
            (search-for-primes (+ start 2) (- n-primes 1)) ; found one!
            (search-for-primes (+ start 2) n-primes)))))   ; keep searching...

; Test code: Use LARGE numbers to get bigger times
; (search-for-primes 1e09 3)
; (search-for-primes 1e10 3)
; (search-for-primes 1e11 3)
; (search-for-primes 1e12 3)

;------------------------------------------------------------------------------- 
;        Output
;-------------------------------------------------------------------------------
; load "(load "ex1_22.scm")
;
; ;Loading "ex1_22.scm"...                  DIFFERENTIAL (sqrt(10) = 3.162278)
; 1000000007. *** .05999999999999872        |
; 1000000009. *** .05999999999999872        |-
; 1000000021. *** .05999999999999872        |
; 10000000019. *** .18999999999999773       T
; 10000000033. *** .19000000000000128       |= 3.222222
; 10000000061. *** .1999999999999993        |
; 100000000003. *** .6000000000000014       T
; 100000000019. *** .5899999999999999       |= 3.086207
; 100000000057. *** .5999999999999979       |
; 1000000000039. *** 1.9299999999999997     T
; 1000000000061. *** 1.9299999999999997     |= 3.223464
; 1000000000063. *** 1.9100000000000001     |
; ;... done
; ;Value 3: "DONE"

;;==============================================================================
;;==============================================================================
