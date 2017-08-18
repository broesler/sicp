;;==============================================================================
;;     File: ex3_55.scm
;;  Created: 08/17/2017, 21:46
;;   Author: Bernie Roesler
;;
;;  Description: 
;;
;;==============================================================================

;;; Exercise 3.55.  Define a procedure partial-sums that takes as argument a stream
;;; S and returns the stream whose elements are S0, S0 + S1, S0 + S1 + S2, ... 
;;; For example, (partial-sums integers) should be the stream 1, 3, 6, 10, 15, ...

(define (partial-sums s)
  (cons-stream
    (stream-car s)
    (add-streams (stream-cdr s)
                 (partial-sums s))))

;;; Enumerate items in a sequence
(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(define irange (enumerate-interval 0 3))
(for-each (lambda (x) (printval (stream-ref (partial-sums integers) x)))
          irange)
; Value: 1
; Value: 3
; Value: 6
; Value: 10

;;==============================================================================
;;==============================================================================
