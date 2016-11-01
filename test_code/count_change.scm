;;==============================================================================
;;     File: count_change.scm
;;  Created: 10/28/2016, 01:32
;;   Author: Bernie Roesler
;;
;;  Description: Counts change
;;
;;==============================================================================

; Call this procedure
(define (count-change amount)
  (cc amount 5)) ; 5 == number of kinds of coins

; Workhorse tree-recursion to implement reduction rule
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

; Define kinds of coins
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; (define (first-denomination kinds-of-coins)
;   (cond ((= kinds-of-coins 1) 1)
;         ((= kinds-of-coins 2) 5)))
;;==============================================================================
;;==============================================================================
