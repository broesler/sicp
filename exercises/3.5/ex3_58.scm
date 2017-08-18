;;==============================================================================
;;     File: ex3_58.scm
;;  Created: 08/18/2017, 11:49
;;   Author: Bernie Roesler
;;
;;  Description: Expansion
;;
;;==============================================================================
(load "streams.scm")

;; Decimal expansion of 1/7 = 0.142857142857...
(define s (expand 1 7 10))

(define irange (enumerate-interval 0 5))
(display "; 1/7 =") 
(for-each (lambda (x) (printval (stream-ref s x))) irange)
(newline)

(define s (expand 3 8 10))
(display "; 3/8 =") 
(for-each (lambda (x) (printval (stream-ref s x))) irange)

;;; Output:
; 1/7 =
; Value: 1
; Value: 4
; Value: 2
; Value: 8
; Value: 5
; Value: 7
; 3/8 =
; Value: 3
; Value: 7
; Value: 5
; Value: 0
; Value: 0
; Value: 0

;;==============================================================================
;;==============================================================================
