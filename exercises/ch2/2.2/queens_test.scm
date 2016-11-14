;;==============================================================================
;;     File: queens_test.scm
;;  Created: 11/13/2016, 23:18
;;   Author: Bernie Roesler
;;
;;  Description: 
;;
;;==============================================================================
(define (queens board-size) 
  (define (queen-cols k) 
    (if (= k 0) 
      (list empty-board) 

      (filter 
        ; This checks if the current queen is safe from the rest of the queens 
        (lambda (positions) (safe? k positions)) 

        (flatmap 
          (lambda (rest-of-queens) 
            (map (lambda (new-row) 
                   (adjoin-position new-row k rest-of-queens)) 
                 (enumerate-interval 1 board-size))) 
          (queen-cols (- k 1)))))) 

  (queen-cols board-size)) 

; Adds a new row to the board (list) 
(define (adjoin-position new-row k rest-of-queens) 
  (cons new-row rest-of-queens)) 

(define empty-board '()) 

; current queen is at start of positions, check it against rest of the queens 
(define (safe? k positions) 
  ; current queen is at start of positions list 
  ; (define queenPos (car positions)) 
  (let ((queenPos (car positions)))

  ; top and bot are used to check for diagonal entries 
  (define (safe-iter top bot remain) 
    (cond ((null? remain) 
           #t) 

          ; Checks for same row and diagonals 
          ((or (= (car remain) queenPos) 
               (= (car remain) top) 
               (= (car remain) bot)) 
           #f) 

          (else 
            (safe-iter (- top 1) (+ bot 1) (cdr remain)))))

  (safe-iter (- queenPos 1) (+ queenPos 1) (cdr positions)))) 

;;; Test code:
(printval (queens 4)) ; Value: 
;;==============================================================================
;;==============================================================================
