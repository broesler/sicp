;;==============================================================================
;;     File: ex2_35.scm
;;  Created: 11/12/2016, 19:08
;;   Author: Bernie Roesler
;;
;;  Description: count-leaves as an accumulation 
;;
;;==============================================================================
(load "sequence_operations.scm")

;;; Define count-leaves in terms of accumulate
(define (count-leaves t)
  (accumulate +
              0 
              (map (lambda (x) 
                     (if (pair? x)
                       (count-leaves x)
                       1))
                   t)))

;; Explanation: (map) gives a list with counts of all sub-trees ("flattening"
;; the tree), and accumulate just sums these counts

;; Test code:
(define x (cons (list 1 2) (list 3 4)))
(printval x)                ; Value: ((1 2) 3 4)
(printval (length x))       ; Value: 3
(printval (count-leaves x)) ; Value: 4

;;==============================================================================
;;==============================================================================
