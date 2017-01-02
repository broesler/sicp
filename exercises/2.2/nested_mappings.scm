;;==============================================================================
;;     File: nested_mappings.scm
;;  Created: 11/13/2016, 18:07
;;   Author: Bernie Roesler
;;
;;  Description: Nested mappings example
;;    Consider this problem: Given a positive integer n, find all ordered pairs
;;    of distinct positive integers i and j, where 1 <= j < i <= n, such that
;;    i + j is prime.
;;
;;==============================================================================
(load "sequence_operations.scm") ; for accumulate, filter, enumerate-interval
(load "../../ch1/1.2/ex1_22-24.scm") ; for (prime?)

;;; Steps to solve:
;;;   1. get sequence of all ordered pairs of positive integers <= n
;;;   2. filter to select those pairs whose sum is prime
;;;   3. for each pair that gets through the filter, generate triple (i,j,i+j)

;;; 1. Generate sequence of pairs (i,j)
;;; (accumulate append
;;;             nil
;;;             (map (lambda (i) 
;;;                    (map (lambda (j) (list i j))
;;;                         (enumerate-interval 1 (- i 1))))
;;;                  (enumerate-interval 1 n)))

;;; combination of mapping and accumulating with append is common, so define:
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;;; 2. predicate i+j == prime?
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

;;; 3. generate triple (i,j,i+j) 
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;;; Combine all steps for final procedure:
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                 (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 n)))))

;;; Test code:
; (printval (prime-sum-pairs 6))
; Value: ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))

;------------------------------------------------------------------------------- 
;        permutations
;-------------------------------------------------------------------------------
;;; For each item x in S, adjoin x to recursive set of all permutations of S - x 
(define (permutations s)
  (if (null? s)                  ; empty set?
    (list nil)                   ; sequence containing empty set
    (flatmap (lambda (x)
               (map (lambda (p) (cons x p))
                    (permutations (remove x s))))
             s)))

;;; filter for S - x
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

; (printval (permutations (list 1 2 3)))
; Value: ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))

;;==============================================================================
;;==============================================================================
