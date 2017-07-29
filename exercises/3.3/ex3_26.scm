;;==============================================================================
;;     File: ex3_26.scm
;;  Created: 01/16/2017, 23:11
;;   Author: Bernie Roesler
;;
;;  Description: Implement tables as trees
;;
;;==============================================================================

;-------------------------------------------------------------------------------
;        Table as a tree
;-------------------------------------------------------------------------------
(define (make-table compare-keys)
  (let ((local-table (list '*table*)))
    ;; Tree procedures from ../2.3/set-tree.scm
    (define (entry tree) (car tree)) 
    (define (left-branch tree) (cadr tree)) 
    (define (right-branch tree) (caddr tree)) 
    (define (make-tree entry left right) 
      (list entry left right)) 

    ;;; Add a leaf (key . value)
    (define (adjoin-set x set)
      (if (null? set) 
        (make-tree x '() '())
        (let ((c (compare-keys (car x) (car (entry set)))))
          (cond ((= c 0) set)
                ((= c -1)
                 (make-tree (entry set)
                            (adjoin-set x (left-branch set))
                            (right-branch set)))
                ((= c 1)
                 (make-tree (entry set)
                            (left-branch set)
                            (adjoin-set x (right-branch set))))))))

    ;; Find record associated with given key
    (define (assoc key records)
      (if (null? records) 
        false
        (let ((c (compare-keys key (car (entry records)))))
          (cond ((= c 0)
                 (entry records))
                ((= c -1)
                 (assoc key (left-branch records)))
                ((= c 1)
                 (assoc key (right-branch records)))))))

    ;; Lookup function for a tree data structure
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
          (cdr record)
          false)))

    ;; Insert an entry at the given location
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
          (set-cdr! record value)
          (set-cdr! local-table 
                    (adjoin-set (cons key value) (cdr local-table)))))
      'ok)

    ;; Dispatch proper procedure to local table
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print) local-table)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;;; Custom key comparison function
(define (compare x y)
  (cond ((< x y) -1)
        ((= x y) 0)
        ((> x y) 1)))

;------------------------------------------------------------------------------- 
;        Test code:
;-------------------------------------------------------------------------------
(define my-table (make-table compare))
(define get (my-table 'lookup-proc))
(define put (my-table 'insert-proc!))

(put 1 'a)
(put 2 'b)
(put 3 'c)
(put 4 'd)
(put 5 'e)
(my-table 'print)
;;; NOTE: Unbalanced tree!! Random order would balance better
; (*table* (1 . a) () ((2 . b) () ((3 . c) () ((4 . d) () ((5 . e) () ())))))
(get 1) ; Value: a
;;==============================================================================
;;==============================================================================
