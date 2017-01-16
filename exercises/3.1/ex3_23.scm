;;==============================================================================
;;     File: ex3_23.scm
;;  Created: 01/16/2017, 15:13
;;   Author: Bernie Roesler
;;
;;  Description: Deque implementation using pairs
;;
;;==============================================================================
(load "ex3_16-19.scm") ; has-cycle?

;------------------------------------------------------------------------------- 
;        Pointer procedures
;-------------------------------------------------------------------------------
;;; Selectors
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))

;;; Mutators
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

;------------------------------------------------------------------------------- 
;        Deque procedures
;-------------------------------------------------------------------------------
;;; Constructor
(define (make-deque) (cons '() '()))

;;; Selectors
(define (empty-deque? deque) 
  (null? (front-ptr deque)))
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (car (rear-ptr deque))))

;;; Mutators
(define (insert-front-deque! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (set-cdr! new-pair (front-ptr deque))
           (set-front-ptr! deque new-pair)
           deque))))

(define (insert-rear-deque! deque item) ; same as original insert
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (set-cdr! (rear-ptr deque) new-pair)
           (set-rear-ptr! deque new-pair)
           deque)))) 

(define (delete-front-deque! deque) ; same as original delete
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (set-front-ptr! deque (cdr (front-ptr deque)))
         deque))) 

(define (delete-rear-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        ((eq? (front-ptr deque) (rear-ptr deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '()))
        (else 
          (let ((p (penultimate-ptr deque)))
            (set-cdr! p '())
            (set-rear-ptr! deque p))
         deque))) 

(define (penultimate-ptr deque)
  (let ((temp (front-ptr deque)))
    (cond ((or (null? temp) 
               (null? (cdr temp)))
           temp)
          ((eq? (cdr temp) (rear-ptr deque))
           temp)
          (else 
            (penultimate-ptr (cdr temp))))))

;;; Auxiliary function
(define (print-deque q)
  (cond ((not (has-cycle? q))
    (newline)
    (display (front-ptr q)))))

;------------------------------------------------------------------------------- 
;        Ex 3.12:
;-------------------------------------------------------------------------------
(define q1 (make-deque))
(insert-rear-deque! q1 'a)  ; Value: ((a) a)
(insert-rear-deque! q1 'b)  ; Value: ((a b) b)
(insert-front-deque! q1 'c) ; Value: ((c a b) b)
(delete-front-deque! q1)    ; Value: ((a b) b)
(delete-rear-deque! q1)     ; Value: ((a) a)
(delete-rear-deque! q1)     ; Value: (() b)
(empty-deque? q1)           ; Value: #t
;;==============================================================================
;;==============================================================================
