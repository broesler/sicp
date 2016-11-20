;;==============================================================================
;;     File: ex2_61-62.scm
;;  Created: 11/20/2016, 17:11
;;   Author: Bernie Roesler
;;
;;  Description: adjoint and union of ordered sets
;;
;;==============================================================================
(load "set-ordered.scm")

;------------------------------------------------------------------------------- 
;        Ex. 61 adjoint
;-------------------------------------------------------------------------------
(define (adjoin-set x set)
  (cond ((element-of-set? x set)
         set)
        ((or (null? set) 
             (< x (car set)))
         (cons x set))
        (else 
          (cons (car set) 
                (adjoin-set x (cdr set))))))

;------------------------------------------------------------------------------- 
;        Ex. 62 union in O(n) time
;-------------------------------------------------------------------------------
(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2))) 
                (cond ((= x1 x2)
                       (cons x1 (union-set (cdr set1) (cdr set2))))
                      ((< x1 x2)
                       (cons x1 (union-set (cdr set1) set2)))
                      ((> x1 x2)
                       (cons x2 (union-set set1 (cdr set2)))))))))

;;; Test code:
(define set1 '(2 3 6 10))
(define set2 '(1 5 6 7))
(printval (adjoin-set 1  set1))         ; Value: (1 2 3 6 10)
(printval (adjoin-set 4  set1))         ; Value: (2 3 4 6 10)
(printval (adjoin-set 11 set1))         ; Value: (2 3 6 10 11)
(printval (intersection-set set1 set2)) ; Value: (6)
(printval (union-set set1 set2))        ; Value: (1 2 3 5 6 7 10)
;;==============================================================================
;;==============================================================================
