;;==============================================================================
;;     File: ex3_47.scm
;;  Created: 04/06/2017, 17:39
;;   Author: Bernie Roesler
;;
;;  Description: semaphore (size n) mutex 
;;
;;==============================================================================
(load "serializer.scm") ; make-serializer, -mutex, etc.

;;; (a) semaphore in terms of mutexes
(define (make-semaphore n)
  (let ((count n) ; n available processes
        (the-mutex (make-mutex)))
    (define (the-semaphore m)
      (cond ((eq? (m 'acquire))
             (the-mutex 'acquire) ; protect count operations
             (if (zero? count)
               (begin (the-mutex 'release)
                      (the-semaphore 'acquire))
               (begin (set! count (- count 1))
                      (the-mutex 'release)))
             ((eq? m 'release)
              (the-mutex 'acquire)
              (if (= count n)
                (the-mutex 'release)
                (begin (set! count (+ count 1))
                       (the-mutex 'release))))))))
  the-semaphore)

;;; (b) semaphore in terms of atomic test-and-set! operations
(define (make-semaphore n)
  (let ((count n) ; n available processes
        (cell (list false))
    (define (the-semaphore m)
      (cond ((eq? (m 'acquire))
             (if (test-and-set! cell) ; protect count operations
               (the-semaphore 'acquire)
               (if (zero? count)
                  (clear! cell)
                  (begin (set! count (- count 1)) 
                         (clear! cell))))
             ((eq? m 'release)
              (if (test-and-set! cell)
                (the-semaphore 'release)
                (if (= count n)
                  (clear! cell)
                  (begin (set! count (+ count 1))
                         (clear! cell))))))))
  the-semaphore)


;;==============================================================================
;;==============================================================================
