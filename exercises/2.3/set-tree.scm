;;==============================================================================
;;     File: set-tree.scm
;;  Created: 11/20/2016, 17:59
;;   Author: Bernie Roesler
;;
;;  Description: Sets as binary trees
;;
;;==============================================================================
;;; Constructor:
(define (make-tree entry left right)
  (list entry left right))

;;; Selectors:
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

;;; Search the tree
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

;;; Add a leaf
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;;; Test code:
; (define tree1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
; (printval (element-of-set? 6 tree1)) ; Value: #t
; (printval (adjoin-set 6 tree1))
; Value: (7 (3 (1 () ()) (5 () (6 () ()))) (9 () (11 () ())))
; (define tree2 (adjoin-set 6 tree1))
;;==============================================================================
;;==============================================================================
