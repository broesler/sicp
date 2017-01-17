;;==============================================================================
;;     File: ex3_24.scm
;;  Created: 01/16/2017, 17:38
;;   Author: Bernie Roesler
;;
;;  Description: Generalize to arbitrary dimension tables, keys are a list
;;
;;==============================================================================

;; Find record associated with given key
(define (assoc key records)
  (cond ((null? records) false)
        ((same-key? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

;;; Redefine local tables with a same-key? argument
(define (make-table same-key?)
  (let ((local-table (list '*table*)))

    ;; Find record at coordinates provided
    (define (lookup keys)
      (define (loop keys subtable)
        (if (null? keys)
          (cdr subtable)
          (let ((new-subtable (assoc (car keys) (cdr subtable))))
            (if new-subtable
              (loop (cdr keys) new-subtable)
              false))))
      (loop keys local-table))

    ;; Insert an entry at the given location
    (define (insert! keys value)
      (define (loop keys subtable)
        (if (null? keys)
          (set-cdr! subtable value) ; found a record
          (let ((new-subtable (assoc (car keys) (cdr subtable))))
              (if new-subtable
                (loop (cdr keys) new-subtable)
                (set-cdr! subtable 
                          (cons (create-subtable keys)
                                (cdr subtable)))))))
      ;; List all keys, except last one is a pair with the value
      (define (create-subtable keys)
        (if (null? (cdr keys))
          (cons (car keys) value)
          (list (car keys) (create-subtable (cdr keys)))))
      (loop keys local-table)
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
(put '(1 1) 7)
(printval (get '(1    1))) ; Value: 7
(printval (get '(1.01 1))) ; Value: 7
(printval (get '(1.1  1))) ; Value: #f

(define same-key? equal?)
(put '(a b c d) 8)
(printval (get '(a b c d))) ; Value: 8
(put '(e f)
     (lambda (x) 
       (+ x 
          (get '(1 1)) 
          (get '(a b c d)))))
(printval ((get '(e f)) 5)) ; Value: 20
;;==============================================================================
;;==============================================================================
