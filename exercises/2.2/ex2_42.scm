;;==============================================================================
;;     File: ex2_42.scm
;;  Created: 11/13/2016, 19:17
;;   Author: Bernie Roesler
;;
;;  Description: Eight queens puzzle (all queens are safe)
;;    One solution for an 8x8 board is:
;;
;;       0 0 0 0 0 1 0 0
;;       0 0 1 0 0 0 0 0
;;       1 0 0 0 0 0 0 0
;;       0 0 0 0 0 0 1 0   ===>>> represent as (3 7 2 8 5 1 4 6)
;;       0 0 0 0 1 0 0 0
;;       0 0 0 0 0 0 0 1
;;       0 1 0 0 0 0 0 0
;;       0 0 0 1 0 0 0 0
;;
;;==============================================================================
(load "sequence_operations.scm")

;;; map but don't create recursive list
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;;; Solve queens puzzle
(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? positions))
        (flatmap (lambda (rest-of-queens)
                   (map (lambda (new-row)
                          (adjoin-position new-row rest-of-queens))
                        (enumerate-interval 1 board-size)))
                 (queen-cols (- k 1))))))
  (queen-cols board-size))

;;; Definitions:
;;; queen-cols: returns the sequence of all ways to place queens in the first
;;;   k columns of the board
;;; rest-of-queens: is a way to place k-1 queens in the first k-1 columns
;;; new-row: proposed row in which to place the queen for the kth column

;------------------------------------------------------------------------------- 
;        Solutions:
;-------------------------------------------------------------------------------
;;; adjoin-position: adjoins a new row-column position to a set of positions
(define (adjoin-position new-row rest-of-queens)
  (cons new-row rest-of-queens))

;;; empty-board: represents an empty set of positions
(define empty-board nil)

;;; safe?: given a set of positions, determines whether the queen in the kth
;;;   column is safe wrt the others (NOTE only need to check if new queen is
;;;   safe, all others are guaranteed safe already)
(define (safe? positions)
  (let ((this-queen (car positions))) ; just added this queen
    (define (safe-iter top bot remain) 
        (cond ((null? remain) 
               #t) 
              ((let ((other-queen (car remain)))
               (or (= other-queen this-queen) ; check this row
                   (= other-queen top)   ; upper diagonal
                   (= other-queen bot))) ; lower diagonal
               #f) 
              (else 
                (safe-iter (- top 1) (+ bot 1) (cdr remain))))) 
    (safe-iter (- this-queen 1) (+ this-queen 1) (cdr positions))))

;;; Test code:
(printval (queens 4)) ; Value: ((3 1 4 2) (2 4 1 3))
;;==============================================================================
;;==============================================================================
