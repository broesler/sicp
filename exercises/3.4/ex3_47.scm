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
  (let ((the-mutex (make-mutex))
        (taken 0))
    (define (acquire)
      (the-mutex 'acquire) ; protect count operations
      (cond ((< taken n) 
             (set! taken (+ taken 1)) ; acquire a semaphore
             (the-mutex 'release))   ; enable other threads to access semaphore
            (else 
              (the-mutex 'release)    ; release first so others can release semaphore
              (acquire))))            ; try again until someone else releases
    (define (release)
      (the-mutex 'acquire)
      (if (zero? taken)
        (error "This semaphore is not yet acquired!")
        (set! taken (- taken 1)))  ; one more available now
      (the-mutex 'release))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire))
            ((eq? m 'release) (release))
            (else (error "unknown request" m))))
    the-semaphore))

;;; (b) semaphore in terms of atomic test-and-set! operations
(define (make-semaphore n)
  (let ((cell (list #t))
        (taken 0))
    (define (acquire)
      (if (test-and-set! cell)
        (acquire)
        (cond ((< taken n)
               (set! taken (+ taken 1))
               (clear! cell))
              (else
                (clear! cell)
                (acquire)))))
    (define (release)
      (cond ((test-and-set! cell)
             (release))
            (else
              (if (zero? taken)
                (error "This semaphore is not yet acquired!")
                (set! taken (- taken 1)))
              (clear! cell))))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire))
            ((eq? m 'release) (release))
            (else (error "unknown request" m))))
  the-semaphore))

;;==============================================================================
;;==============================================================================
