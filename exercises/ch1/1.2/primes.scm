;;==============================================================================
;;     File: primes.scm
;;  Created: 11/03/2016, 13:25
;;   Author: Bernie Roesler
;;
;;  Description: Testing for primality
;;
;;==============================================================================

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
;        Fermat test
;-------------------------------------------------------------------------------
; primitive remainder
(define (even? n)
  (= (remainder n 2) 0))

; Θ(log n)
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

; Fermat test (run once)
(define (fermat-test n)
  (define (try-it a)                ; try a random base a < n
    (= (expmod a n n) a))           ; is a^n mod n == a?
  (try-it (+ 1 (random (- n 1)))))  ; get random a \in [1,n]

; faster prime test (run Fermat test a given number of times)
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
;;==============================================================================
;;==============================================================================
