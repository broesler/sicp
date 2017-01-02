;;==============================================================================
;;     File: ex2_19.scm
;;  Created: 11/09/2016, 23:56
;;   Author: Bernie Roesler
;;
;;  Description: change counting with lists 
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

;------------------------------------------------------------------------------- 
;        Rewrite with lists
;-------------------------------------------------------------------------------
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
; Call, i.e. (cc 100 us-coins)

; redefine cc
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;;; Define aux functions
(define (no-more? items) (null? items))
(define (except-first-denomination coin-values) (cdr coin-values))
(define (first-denomination coin-values) (car coin-values))

;; Test code:
(printval (cc 100 us-coins))           ; Value: 292
(printval (cc 100 (reverse us-coins))) ; Value: 292 order doesn't matter!
; (printval (cc 100 uk-coins)) ; Value: 104561 (SUPER slow tree-recursive)
;;==============================================================================
;;==============================================================================
