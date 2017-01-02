;;==============================================================================
;;     File: ex2_40.scm
;;  Created: 11/13/2016, 18:23
;;   Author: Bernie Roesler
;;
;;  Description: Find unique pairs to simplify prime-sum-pairs procedure
;;
;;==============================================================================
(load "nested_mappings.scm")

;;; Given integer n, generate sequence of pairs (i,j) with 1 <= j < i <= n
(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

;;; Redefine prime-sum-pairs using our new definition
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(printval (prime-sum-pairs 6))
; Value: ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))
;;==============================================================================
;;==============================================================================
