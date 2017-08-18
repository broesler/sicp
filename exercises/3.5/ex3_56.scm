;;==============================================================================
;;     File: ex3_56.scm
;;  Created: 08/17/2017, 22:11
;;   Author: Bernie Roesler
;;
;;  Description: Hamming problem
;;
;;==============================================================================
(load "streams.scm")

;;; A famous problem, first raised by R. Hamming, is to enumerate, in ascending
;;; order with no repetitions, all positive integers with no prime factors other
;;; than 2, 3, or 5. One obvious way to do this is to simply test each integer
;;; in turn to see whether it has any factors other than 2, 3, and 5. But this
;;; is very inefficient, since, as the integers get larger, fewer and fewer of
;;; them fit the requirement. As an alternative, let us call the required stream
;;; of numbers S and notice the following facts about it.
;;;   -- S begins with 1.
;;;   -- The elements of (scale-stream S 2) are also elements of S.
;;;   -- The same is true for (scale-stream S 3) and (scale-stream 5 S).
;;;   -- These are all the elements of S.

(define S (cons-stream 1 (merge (scale-stream S 2) 
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))

;;; Enumerate items in a sequence
(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(define irange (enumerate-interval 0 10))
(for-each (lambda (x) (printval (stream-ref S x)))
          irange)

; Value: 1
; Value: 2
; Value: 3
; Value: 4
; Value: 5
; Value: 6
; Value: 8
; Value: 9
; Value: 10
; Value: 12

;;==============================================================================
;;==============================================================================
