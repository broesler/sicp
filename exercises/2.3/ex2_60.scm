;;==============================================================================
;;     File: ex2_60.scm
;;  Created: 11/20/2016, 16:01
;;   Author: Bernie Roesler
;;
;;  Description: Sets allowing duplicates 
;;
;;==============================================================================

;;; i.e. {1,2,3} could be (2 3 2 1 3 2 2)

;;; Doesn't change!
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;;; Duplicates allowed, so just cons it on, time is O(1)
(define (adjoin-set x set)
  (cons x set))

;;; Intersection doesn't change! We just get duplicates in the output set
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;;; Union is just straight append because we don't need to check for existence
;;; Time is O(n)
(define (union-set set1 set2)
  (append set1 set2))

;;; Test code:
(define set1 '(1 1 5 3 5 2 2 3 4 4))
(define set2 '(3 3 2 2 4 7 4 6 7 7))
(printval (element-of-set? 4 set1))     ; Value: #t
(printval (adjoin-set 4 set2))          ; Value: (4 3 3 2 2 4 7 4 6 7 7)
(printval (intersection-set set1 set2)) ; Value: (3 2 2 3 4 4) 
(printval (union-set set1 set2))
; Value: (1 1 5 3 5 2 2 3 4 4 3 3 2 2 4 7 4 6 7 7)
;;==============================================================================
;;==============================================================================
