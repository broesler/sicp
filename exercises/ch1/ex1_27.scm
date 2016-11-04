;;==============================================================================
;;     File: ex1_27.scm
;;  Created: 11/03/2016, 18:57
;;   Author: Bernie Roesler
;;
;;  Description: Test if Carmichael numbers pass the Fermat test
;;
;;==============================================================================
(load "ex1_22-24.scm") ; get necessary functions

(define (carmichael-test n)
  (carm-iter n (- n 1)))

(define (carm-iter n a)
  (cond ((= a 0) "Prime!")
        ((cong-mod-n? n a) (carm-iter n (- a 1))) ; if congruent, keep checking
        (else "Not prime!")))

(define (cong-mod-n? n a)
    (= (expmod a n n) a))           ; is a^n mod n == a?

; Pretty-print our results
(define (print-result x op1 op2)
  (newline)
  (display "; n = ")
  (display x)
  (display " ; ")
  (display (op1 x))
  (display " ; smallest-divisor: ")
  (display (op2 x)))

; Test code: Base cases, plut first few Carmichael numbers
(for-each (lambda (x) (print-result x carmichael-test smallest-divisor)) 
     '(7 8 561 1105 1729 2465 2821 6601))

; Carmichael numbers PASS the test, but n != smallest-divisor!!
;;==============================================================================
;;==============================================================================
