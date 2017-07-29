;;==============================================================================
;;     File: ex2_59.scm
;;  Created: 11/20/2016, 15:27
;;   Author: Bernie Roesler
;;
;;  Description: union-set operation 
;;
;;==============================================================================
(load "set-unordered.scm")

;;; Union of two sets == set of elements that appear in either set
;;; Type: (set, set) -> set
(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1) 
               (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))

;;; Test code:
(printval (union-set set1 set2)) ; Value: (1 5 3 2 4 7 6)
;;==============================================================================
;;==============================================================================
