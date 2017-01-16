;;==============================================================================
;;     File: ex3_22.scm
;;  Created: 01/16/2017, 14:21
;;   Author: Bernie Roesler
;;
;;  Description: Queues with local state 
;;
;;==============================================================================

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
        (error "FRONT called with an empty queue")
        (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               (cons front-ptr rear-ptr))
              (else
                (set-cdr! rear-ptr new-pair)
                (set-rear-ptr! new-pair)
                (cons front-ptr rear-ptr)))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue" front-ptr))
            (else
              (set-front-ptr! (cdr front-ptr))
              (cons front-ptr rear-ptr))))
    (define (print-queue)
      (newline)
      (display front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue) print-queue)
            (else (error "MAKE-QUEUE unrecognized procedure!"))))
    dispatch))

;;; Test code:
(define q1 (make-queue))
(printval ((q1 'empty-queue?)))     ; Value: #t
(printval ((q1 'insert-queue!) 'a)) ; Value: ((a) a)
(printval ((q1 'insert-queue!) 'b)) ; Value: ((a b) b)
(printval ((q1 'front-queue)))      ; Value: a
((q1 'print-queue))                 ; (a b)
(printval ((q1 'delete-queue!)))    ; Value: ((b) b)
(printval ((q1 'delete-queue!)))    ; Value: (() b)

;;; NOTE: could not return anything from the various functions because we are
;;; altering internal state. Just use ((q1 'print-queue)) to view state.

;;==============================================================================
;;==============================================================================
