;;==============================================================================
;;     File: ex1_28.scm
;;  Created: 11/03/2016, 19:28
;;   Author: Bernie Roesler
;;
;;  Description: Miller-Rabin test (non-foolable Fermat test)
;;
;;==============================================================================
(load "ex1_22-24.scm") ; get Fermat test as well

;------------------------------------------------------------------------------- 
;        Miller-Rabin test should not get fooled
;-------------------------------------------------------------------------------
; primitive remainder
(define (even? n)
  (= (remainder n 2) 0))

; Modify expmod to check for "non-trivial square root of 1 mod n"
; Θ(log n) form of calculating b^n mod m
(define (expmod b n m)
  (cond ((= n 0) 1)
        ((even? n)
         (let ((x (expmod b (/ n 2) m)))                  ; with let
         (if (non-trivial-sqrt? x m) 0 (remainder (square x) m))))
         ; (if (non-trivial-sqrt? (expmod b (/ n 2) m) m)     ; without let
         ;   0 
         ;   (remainder (square (expmod b (/ n 2) m)) m)))
         ; (remainder (square (expmod b (/ n 2) m))         ; original code:
         ;            m))
        (else
          (remainder (* b (expmod b (- n 1) m))
                     m))))

; non-trivial square root check
(define (non-trivial-sqrt? x m)
  (cond ((= x 1) false)
        ((= x (- m 1)) false)
        (else (= (remainder (square x) m) 1)))) ; x^2 is congruent with 1 mod n?

; Fermat test (for a single integer)
(define (miller-rabin-test n)
  (define (try-it a)                ; try a random base a < n
    (= (expmod a (- n 1) n) 1))     ; is a^(n-1) mod n == 1?
  (try-it (+ 1 (random (- n 1)))))  ; get random a \in [1,n]

; fast prime test (run Miller-Rabin test a given number of times)
(define (fast-prime-mr? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime-mr? n (- times 1)))
        (else false)))

; Test code (only run twice to increase chance of Carmichael numbers failing)
(display "Fermat test:\n")
(display (map (lambda (x) (fast-prime? x 2)) 
     '(7 8 13 17 561 1105 1729 2465 2821 6601)))
(newline)

; In all Miller-Rabin tests, Carmichael numbers return "#f" (not prime)
(display "Miller-Rabin test:\n")
(display (map (lambda (x) (fast-prime-mr? x 2)) 
     '(7 8 13 17 561 1105 1729 2465 2821 6601)))
;;==============================================================================
;;==============================================================================
