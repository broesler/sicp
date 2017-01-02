;;==============================================================================
;;     File: ex2_30-31.scm
;;  Created: 11/12/2016, 12:59
;;   Author: Bernie Roesler
;;
;;  Description: tree mapping
;;
;;==============================================================================

;------------------------------------------------------------------------------- 
;        Ex 2.30 square-tree
;-------------------------------------------------------------------------------
;;; Direct definition
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree)) ; act on sub-tree
                    (square-tree (cdr tree))))))

;;; Define using map
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (not (pair? sub-tree))
           (square sub-tree)
           (square-tree sub-tree)))
       tree))

;;; Test code:
(define x 
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
(printval (square-tree x))
; Value: (1 (4 (9 16) 25) (36 49))

;------------------------------------------------------------------------------- 
;        Ex 2.31
;-------------------------------------------------------------------------------
;;; abstract above to produce tree-map (without using higher-order procedures)
(define (tree-map f tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (f tree))
        (else (cons (tree-map f (car tree))
                    (tree-map f (cdr tree))))))

;;; abstract above to produce tree-map (using map + recursion)
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (not (pair? sub-tree))
           (f sub-tree)
           (tree-map f sub-tree)))
       tree))

;;; redefine square-tree in terms of abstraction
(define (square-tree tree) (tree-map square tree))

;;; Test code:
(define x 
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
(printval (square-tree x))
; Value: (1 (4 (9 16) 25) (36 49))

;;==============================================================================
;;==============================================================================
