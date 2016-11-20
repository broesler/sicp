;;==============================================================================
;;     File: set.scm
;;  Created: 11/20/2016, 15:22
;;   Author: Bernie Roesler
;;
;;  Description: Representing sets book code
;;
;;==============================================================================

;;; Is object x in set?
;;; Type: (object, set) -> boolean
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;;; Add object x to set
;;; Type: (object, set) -> set
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;;; Intersection of two sets == set of objects that are in both sets
;;; Type: (set, set) -> set
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;;; Test code:
(define set1 '(1 5 3 2 4))
(define set2 '(3 2 4 7 6))
(printval (element-of-set? 4 set1))     ; Value: #t
(printval (adjoin-set 8 set2))          ; Value: (8 3 2 4 7 6)
(printval (intersection-set set1 set2)) ; Value: (3 2 4)
;;==============================================================================
;;==============================================================================
