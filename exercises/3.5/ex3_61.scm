;;==============================================================================
;;     File: ex3_61.scm
;;  Created: 08/18/2017, 13:16
;;   Author: Bernie Roesler
;;
;;  Description: Invert a series with a0 = 1 
;;
;;==============================================================================
(load "ex3_60.scm")

(define (invert-unit-series s)
  (cons-stream 1
               (mul-series (scale-stream (stream-cdr s) -1)
                           (invert-unit-series s))))

;;; Test code:
(define s (list-stream '(1 2 3 4 5)))
(define x (invert-unit-series s))
; (display-stream-n (mul-series s x) 5)
; Value: 1
; Value: 0
; Value: 0
; Value: 0
; Value: 0

;;==============================================================================
;;==============================================================================
