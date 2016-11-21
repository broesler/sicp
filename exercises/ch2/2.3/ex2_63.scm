;;==============================================================================
;;     File: ex2_63-65.scm
;;  Created: 11/20/2016, 18:32
;;   Author: Bernie Roesler
;;
;;  Description: Lists as binary trees
;;
;;==============================================================================
(load "set-ordered.scm")
(load "ex2_61-62.scm") ; union-set and adjoint-set for ordered lists
(load "set-tree.scm")

;------------------------------------------------------------------------------- 
;        Ex 2.63 tree->list
;-------------------------------------------------------------------------------
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;;; Test code:
(define tree1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
; (define tree2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
; (define tree3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))
; (printval (tree->list-1 tree1)) ; Value: (1 3 5 7 9 11)
; (printval (tree->list-1 tree2)) ; Value: (1 3 5 7 9 11)
; (printval (tree->list-1 tree3)) ; Value: (1 3 5 7 9 11)
; (printval (tree->list-2 tree1)) ; Value: (1 3 5 7 9 11)
; (printval (tree->list-2 tree2)) ; Value: (1 3 5 7 9 11)
; (printval (tree->list-2 tree3)) ; Value: (1 3 5 7 9 11)

;------------------------------------------------------------------------------- 
;        Ex. 2.64 list->tree
;-------------------------------------------------------------------------------
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

;;; (list, number) -> (tree . list)
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;;; Test code:
; (printval (list->tree (tree->list-1 tree1)))
; Value: (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

;------------------------------------------------------------------------------- 
;        Ex 2.65 union and intersection of trees
;-------------------------------------------------------------------------------
(define (intersection-set-tree set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (let ((intersection-list (intersection-set list1 list2)))
      (list->tree intersection-list))))

(define (union-set-tree set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (let ((union-list (union-set list1 list2)))
      (list->tree union-list))))

;;; Test code:
(define tree2 (adjoin-set 6 (adjoin-set 8 tree1)))
(printval (intersection-set-tree tree1 tree2))
; Value: (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
(printval (union-set-tree tree1 tree2))
; Value: (6 (3 (1 () ()) (5 () ())) (8 (7 () ()) (9 () (11 () ()))))
;;==============================================================================
;;==============================================================================
