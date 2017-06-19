;;==============================================================================
;;     File: ex3_47.scm
;;  Created: 04/06/2017, 17:39
;;   Author: Bernie Roesler
;;
;;  Description: semaphore (size n) mutex 
;;
;;==============================================================================
(load "serializer.scm") ; make-serializer, -mutex, etc.
;
;;; (a) semaphore in terms of mutexes
;;; From: https://github.com/felix021/sicp/blob/master/code/3-47.scm
(define (make-semaphore-mutex n)
  (let ((the-mutex (make-mutex)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (the-mutex 'acquire) ; protect count operations
             (cond ((= n 0) 
                    (the-mutex 'release) ; release first
                    (the-semaphore m))
                   (else 
                     (set! n (- n 1))
                     (the-mutex 'release)))) ; enable other threads
            ((eq? m 'release)
             (the-mutex 'acquire)
             (set! n (+ n 1))
             (the-mutex 'release))
            (else (error "unknown request" m))))
    the-semaphore))

;;; (b) semaphore in terms of atomic test-and-set! operations
(define (make-semaphore n)
  (let ((cell (list #t)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
               (the-semaphore m)
               (cond ((= n 0)
                      (clear! cell) ; release first
                      (the-semaphore m))
                     (else
                       (set! n (- n 1))
                       (clear! cell))))) ;enable other threads
            ((eq? m 'release)
             (if (test-and-set! cell)
               (the-semaphore m)
               (begin
                 (set! n (+ n 1))
                 (clear! cell))))
            (else (error "unknown request" m))))
    the-semaphore))

;;==============================================================================
;;==============================================================================
