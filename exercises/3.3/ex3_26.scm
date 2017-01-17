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
    (define (left-branch tree) (caar tree)) 
    (define (right-branch tree) (caddr tree)) 
    (define (make-tree entry left right) 
      (list entry left right)) 

    ;; key-value selectors
    (define key car)
    (define val cdr)

    ;;; Add a leaf
    (define (adjoin-set x set)
      (let ((c (compare-keys (key x) (key (entry set)))))
        (cond ((null? set) (make-tree x '() '()))
              ((= c 0) set)
              ((= c -1)
               ; (make-tree (entry set)
               ;            (adjoin-set x (left-branch set))
               ;            (right-branch set)))
               (let ((left (left-branch set))) 
                 (set! left (adjoin-set x left))))
              ((= c 1)
               ; (make-tree (entry set)
               ;            (left-branch set)
               ;            (adjoin-set x (right-branch set)))
               (let ((right (right-branch set)))
                 (set! right (adjoin-set x right)))))))

    ;; Find record associated with given key
    (define (assoc key records)
      (let ((c (compare-keys key (key (entry records)))))
        (cond ((null? records) false)
              ((= c 0)
               (entry records))
              ((= c -1)
               (assoc key (left-branch records)))
              ((= c 1)
               (assoc key (right-branch records))))))

    ;; Lookup function for a tree data structure
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
          (val record)
          false)))

    ;; Insert an entry at the given location
    (define (insert! keys value)
      (let ((record (assoc key (cdr local-table))))
        (if record
          (set-cdr! record value)
          (adjoin-set (cons key value) (cdr local-table))))
      'ok)

    ;; Dispatch proper procedure to local table
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;;; Custom key comparison function
(define (compare x y)
  (cond ((< x y) -1)
        ((= x y) 0)
        ((> x y) 1)))

;;; Test code:
(define my-table (make-table compare))
(define get (my-table 'lookup-proc))
(define put (my-table 'insert-proc!))

(put 1 7)
(printval (get 1)) ; Value: 7
;;==============================================================================
;;==============================================================================
