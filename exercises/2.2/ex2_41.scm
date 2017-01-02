;;==============================================================================
;;     File: ex2_41.scm
;;  Created: 11/13/2016, 18:27
;;   Author: Bernie Roesler
;;
;;  Description: Ordered triples 
;;
;;==============================================================================
(load "sequence_operations.scm")

;;; Find all ordered-triples (i,j,k) of distinct positive integers, less than or
;;; equal to a given integer n, that sum to a given integer s.
(define (ordered-triples-sum n s)
  (define (three-sum? triple)
    (= s (+ (car triple) (cadr triple) (caddr triple))))
  (filter three-sum? (unique-triples n)))

;;; Given integer n, generate sequence of triples (i,j,k) with i,j,k <= n, 
;;; such that i != j != k
(define (unique-triples n)
  (flatmap (lambda (i) 
      (flatmap (lambda (j) 
             (map (lambda (k) (list i j k))
                  (enumerate-interval 1 (- j 1))))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

;;; Test code:
; (unique-triples 3) ; Value: ((3 2 1))
; (unique-triples 4) ; Value: ((3 2 1) (4 2 1) (4 3 1) (4 3 2))
(printval (ordered-triples-sum 4 8)) ; Value: ((4 3 1))
;;==============================================================================
;;==============================================================================
