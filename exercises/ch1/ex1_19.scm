;;==============================================================================
;;     File: ex1_19.scm
;;  Created: 11/03/2016, 00:29
;;   Author: Bernie Roesler
;;
;;  Description: Fibonacci numbers in log time
;;
;;==============================================================================

; initial call
(define (fib n)
  (fib-iter 1 0 0 1 n))

; iteration procedure
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))      ; compute p'
                   (+ (square q) (* 2 p q))       ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

; Test code:
(map (lambda (n) (fib n)) '(0 1 2 3 4 5 6 7 8))
;Value 2: (0 1 1 2 3 5 8 13 21)

;;==============================================================================
;;==============================================================================
