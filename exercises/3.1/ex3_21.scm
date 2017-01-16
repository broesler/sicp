;;==============================================================================
;;     File: ex3_21.scm
;;  Created: 01/16/2017, 14:05
;;   Author: Bernie Roesler
;;
;;  Description: Queue implementation
;;
;;==============================================================================

;;; Queue implementation from Section 3.3.2
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue)))) 

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue))) 

;------------------------------------------------------------------------------- 
;        Ex 3.12:
;-------------------------------------------------------------------------------
(define q1 (make-queue))
(insert-queue! q1 'a) ; Value: ((a) a)
(insert-queue! q1 'b) ; Value: ((a b) b)
(delete-queue! q1)    ; Value: ((b) b)
(delete-queue! q1)    ; Value: (() b)

;;; It works correctly, but the interpreter prints the subcomponents of the list
;;; pointed to by q1, i.e. front-ptr, rear-ptr. The front-ptr points to the
;;; entire queue, so it is shown as the car of the printed list. The rear-ptr
;;; points to the last pair in the queue, so its car is printed as the cdr of
;;; the queue list.

(define (print-queue q)
  (newline)
  (display (front-ptr q)))
; (print-queue q1)
;;==============================================================================
;;==============================================================================
