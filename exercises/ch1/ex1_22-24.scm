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
        ; (else (find-divisor n (+ test-divisor 1)))))  ; original line
        (else (find-divisor n (next test-divisor)))))    ; modified

; Skip evens larger than 2 in find-divisor
(define (next n)
  (if (= n 2)
    3
    (+ n 2)))

(define (divides? a b)
  (= (remainder b a) 0))

; n is prime iff it is its own smallest divisor (other than 1)
(define (prime? n)
  (= n (smallest-divisor n)))

;------------------------------------------------------------------------------- 
;        Fermat test
;-------------------------------------------------------------------------------
; primitive remainder
(define (even? n)
  (= (remainder n 2) 0))

; Θ(log n) form of calculating b^n mod m
(define (expmod b n m)
  (cond ((= n 0) 1)
        ((even? n)
         (remainder (square (expmod b (/ n 2) m))
                    m))
        (else
          (remainder (* b (expmod b (- n 1) m))
                     m))))

; Fermat test (for a single integer)
(define (fermat-test n)
  (define (try-it a)                ; try a random base a < n
    (= (expmod a n n) a))           ; is a^n mod n == a?
  (try-it (+ 1 (random (- n 1)))))  ; get random a \in [1,n]

; faster prime test (run Fermat test a given number of times)
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;------------------------------------------------------------------------------- 
;       Timing/printing function
;-------------------------------------------------------------------------------
; time the call
(define (timed-prime-test n)
  (start-prime-test n (runtime)))

; check for primality
(define (start-prime-test n start-time)
  ; (if (prime? n)                ; smallest-divisor test
  (if (fast-prime? n 1000)       ; Fermat test 
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
; (search-for-primes 1000000000    3) ; 1e09 (need to write out 0's for integer)
; (search-for-primes 10000000000   3) ; 1e10
; (search-for-primes 100000000000  3) ; 1e11
; (search-for-primes 1000000000000 3) ; 1e12

;------------------------------------------------------------------------------- 
;        Output
;-------------------------------------------------------------------------------
; Run before modification of smallest-divisor to skip evens > 2
; ;Loading "ex1_22.scm"...                  DIFFERENTIAL (sqrt(10) = 3.162278)
; 1000000007. *** .05999999999999872        |
; 1000000009. *** .05999999999999872        |-
; 1000000021. *** .05999999999999872        |
; 10000000019. *** .18999999999999773       T
; 10000000033. *** .19000000000000128       |= 3.222222 x
; 10000000061. *** .1999999999999993        |
; 100000000003. *** .6000000000000014       T
; 100000000019. *** .5899999999999999       |= 3.086207 x
; 100000000057. *** .5999999999999979       |
; 1000000000039. *** 1.9299999999999997     T
; 1000000000061. *** 1.9299999999999997     |= 3.223464 x
; 1000000000063. *** 1.9100000000000001     |
; ;... done
; ;Value 3: "DONE"

; Run with modification of smallest-divisor to skip evens > 2
;   Times are ~1/2 of previous run!
; ;Loading "ex1_22.scm"...
; 1000000007. *** 3.0000000000001137e-2
; 1000000009. *** 2.9999999999997584e-2
; 1000000021. *** 2.9999999999997584e-2
; 10000000019. *** .10999999999999943
; 10000000033. *** .10000000000000142
; 10000000061. *** .10999999999999943
; 100000000003. *** .3299999999999983
; 100000000019. *** .3299999999999983
; 100000000057. *** .3200000000000003
; 1000000000039. *** 1.0599999999999987
; 1000000000061. *** 1.0199999999999996
; 1000000000063. *** 1.0599999999999987
; ;... done
; ;Value 5: "DONE"

; Fermat test
; ;Loading "ex1_22.scm"...                  DIFFERENTIAL (log(10) = 1 = const.)
; 1000000007 *** 6.0000000000002274e-2      |
; 1000000009 *** .05999999999999872         |-
; 1000000021 *** .05999999999999872         |
; 10000000019 *** .07000000000000028        T
; 10000000033 *** .07000000000000028        |= 0.01 s
; 10000000061 *** .05999999999999872        |
; 100000000003 *** .07000000000000028       T
; 100000000019 *** .08999999999999986       |= 0.01 s
; 100000000057 *** .0799999999999983        |
; 1000000000039 *** .08000000000000185      T
; 1000000000061 *** .08999999999999986      |= 0.01 s
; 1000000000063 *** .08999999999999986      |
; ;... done
; ;Value 8: "DONE"
;;==============================================================================
;;==============================================================================
