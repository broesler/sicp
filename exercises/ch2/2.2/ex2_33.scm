;;==============================================================================
;;     File: ex2_33.scm
;;  Created: 11/12/2016, 18:24
;;   Author: Bernie Roesler
;;
;;  Description: Rewrite list operations as accumulations
;;
;;==============================================================================

;;; Definitions
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1)) ; cons is a "push" operation

(define (length sequence)
  (accumulate (lambda (x y)
                (if (null? x)  ; accumulate 1 per not null item
                  (+ 0 y)
                  (+ 1 y)))
              0
              sequence))

;;; Test code:
(define x (list 1 2 3 4))
(define y (list 1 2 3))
(printval (map square x)) ; Value: (1 4 9 16)
(printval (append x y))   ; Value: (1 2 3 4 1 2 3)
(printval (length x))     ; Value: 4
;;==============================================================================
;;==============================================================================
