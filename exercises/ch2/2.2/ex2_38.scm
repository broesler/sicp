;;==============================================================================
;;     File: ex2_38.scm
;;  Created: 11/13/2016, 17:43
;;   Author: Bernie Roesler
;;
;;  Description: folding operations
;;
;;==============================================================================

;;; accumulate is "fold-right" because it combines first element with the result
;;; of combining all the elements to the right.
(define fold-right accumulate)

;;; "fold-left" works in the opposite direction
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;;; Guess and check these values:
(printval (fold-right / 1 (list 1 2 3))) ; Value: 3/2
(printval (fold-left / 1 (list 1 2 3))) ; Value: 1/6
(printval (fold-right list nil (list 1 2 3))) ; Value: (1 (2 (3 ())))
(printval (fold-left list nil (list 1 2 3))) ; Value: (((() 1) 2) 3)

;------------------------------------------------------------------------------- 
;       Ex 2.39 reverse in terms of fold-left and fold-right
;-------------------------------------------------------------------------------
;;; fold-right is recursive, so need to append to a list
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(printval (reverse (list 1 2 3))) ; Value: (3 2 1)

;;; fold-left is iterative, so just cons them together in reverse
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(printval (reverse (list 1 2 3))) ; Value: (3 2 1)
;;==============================================================================
;;==============================================================================
