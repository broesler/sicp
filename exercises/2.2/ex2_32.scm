;;==============================================================================
;;     File: ex2_32.scm
;;  Created: 11/12/2016, 13:13
;;   Author: Bernie Roesler
;;
;;  Description: Set of all subsets
;;
;;==============================================================================

;;; Complete this definition (fill in "map" term):
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s)))
            (f (lambda (x) (cons (car s) x))))
        (append rest (map f rest)))))

;;; Test code:
(define s (list 1 2 3))
(printval (subsets s))

;;; We need to (cons) the (car) of the original set with the subsets of the
;;; (cdr) of the original set, which makes a list of subsets. Also included in
;;; the list of subsets are those subsets themselves (without the (car)). If we
;;; repeat this process recursively for each (cdr) reduction step, we return the
;;; set of all subsets.
;;==============================================================================
;;==============================================================================
