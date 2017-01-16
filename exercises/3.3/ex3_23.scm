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
;        Doubly-linked list procedures
;-------------------------------------------------------------------------------
;;; Constructor
(define (make-node val)
  (list val '() '()))

;;; Selectors
(define node-val car)
(define node-prev cadr)
(define node-next caddr)

;;; Mutators
(define (set-node-prev! node p*)
  (define p (cdr node))
  (set-car! p p*))
(define (set-node-next! node p*)
  (define p (cddr node))
  (set-car! p p*))

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
      (node-val (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (node-val (rear-ptr deque))))

;;; Mutators
(define (insert-front-deque! deque item)
  (let ((new-node (make-node item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node))
          (else
           (set-node-prev! (front-ptr deque) new-node)
           (set-node-next! new-node (front-ptr deque))
           (set-front-ptr! deque new-node)))))

(define (insert-rear-deque! deque item) ; same as original insert
  (let ((new-node (make-node item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node))
          (else
           (set-node-prev! new-node (rear-ptr deque))
           (set-node-next! (rear-ptr deque) new-node)
           (set-rear-ptr! deque new-node)))))

(define (delete-front-deque! deque) ; same as original delete
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (set-front-ptr! deque (node-next (front-ptr deque)))
         (if (null? (front-ptr deque))
           (set-rear-ptr! deque (front-ptr deque))
           (set-node-prev! (front-ptr deque) '())))))

(define (delete-rear-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else 
          (set-rear-ptr! deque (node-prev (rear-ptr deque)))
          (if (null? (rear-ptr deque))
            (set-front-ptr! deque (rear-ptr deque))
            (set-node-next! (rear-ptr deque) '())))))

;;; Auxiliary functions
(define (flatten q)
  (define (iter temp)
    (if (null? temp)
      '()
      (cons (node-val temp) 
            (iter (node-next temp)))))
  (iter (front-ptr q)))

(define (print-deque q)
  (newline)
  (display (flatten q)))

;------------------------------------------------------------------------------- 
;        Test code
;-------------------------------------------------------------------------------
(define dq (make-deque))
(insert-rear-deque! dq 'a) (print-deque dq)  ; (a)
(insert-rear-deque! dq 'b) (print-deque dq)  ; (a b)
(insert-front-deque! dq 'c) (print-deque dq) ; (c a b)
(delete-front-deque! dq) (print-deque dq)    ; (a b)
(delete-rear-deque! dq) (print-deque dq)     ; (a)
(delete-rear-deque! dq) (print-deque dq)     ; ()
(empty-deque? dq)                            ; Value: #t
;;==============================================================================
;;==============================================================================
