;;==============================================================================
;;     File: ex3_24.scm
;;  Created: 01/16/2017, 17:38
;;   Author: Bernie Roesler
;;
;;  Description: Table constructor with generic same-key?
;;
;;==============================================================================

;; Find record associated with given key
(define (assoc key records)
  (cond ((null? records) false)
        ; Use same-key? instead of equal? here
        ((same-key? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

;;; Redefine local tables with a same-key? argument
(define (make-table same-key?)
  (let ((local-table (list '*table*)))

    ;; Find record at row/column provided
    (define (lookup key-1 key-2)
      (bkpt 'test)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              false))
          false)))

    ;; Insert an entry at the given location
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
      'ok)    
    ;; Dispatch proper procedure to local table
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;; Build a local table
(define (same-key? x y)
  (let ((tol 0.1))
    (<= (abs (- x y)) tol)))
(define my-table (make-table same-key?))
(define get (my-table 'lookup-proc))
(define put (my-table 'insert-proc!))

;;; Test code:
(put 1 1 7)
(printval (get 1    1)) ; Value: 7
(printval (get 1.01 1)) ; Value: 7
(printval (get 1.1  1)) ; Value: #f
;;==============================================================================
;;==============================================================================
