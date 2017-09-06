;;==============================================================================
;;     File: ex3_81.scm
;;  Created: 09/04/2017, 20:55
;;   Author: Bernie Roesler
;;
;;  Description: Random number generator with streams
;;
;;==============================================================================
(load "streams.scm")
(define random-init 7)
(load "../../sicp_code/ch3support.scm") ; rand-update

;;; rand should take stream of requests
(define requests 
  (list->stream 
    (list 'generate (cons 'reset 9) 'generate 
          'generate (cons 'reset 9) 'generate 'generate)))
;;; Should produce: 88 15 50 15 50 ...

;;; Redefine rand to take arguments 'reset or 'generate
(define (rand seed requests)
  ;; Initialize random numbers stream
  (define (rand-init init-val)
    (cons-stream init-val
                 (rand (rand-update init-val)
                       (stream-cdr requests))))
  (if (stream-null? requests)
    the-empty-stream
    (let ((m (stream-car requests)))
      (cond ((eq? m 'generate)
             (rand-init seed))
            ((and (pair? m) 
                  (eq? (car m) 'reset) 
                  (number? (cdr m)))
             (rand-init (cdr m)))
            (else (error "bad request -- RAND" m))))))

;;; Test code:
(define random-numbers (rand 7 requests))
(display-stream-n random-numbers 7)
    

;;==============================================================================
;;==============================================================================
