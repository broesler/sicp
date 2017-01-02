;;==============================================================================
;;     File: ex2_5.scm
;;  Created: 11/09/2016, 15:28
;;   Author: Bernie Roesler
;;
;;  Description: Pairs as arithmetic operations only
;;
;;==============================================================================

;;; Claim: we can represent a pair of integers (a,b) as 2^a*3^b.

;; Constructor
(define (cons a b) (* (expt 2 a) (expt 3 b)))

;; Selectors
(define (car z) (div-iter z 2 0))
(define (cdr z) (div-iter z 3 0))

;; iterator
(define (div-iter z div count)
  (if (> (remainder z div) 0)
    count
    (div-iter (/ z div) div (+ count 1))))

; Test code:
(define z (cons 5 9))
(display z)
(newline)
(display (car z))
(newline)
(display (cdr z))
;;==============================================================================
;;==============================================================================
