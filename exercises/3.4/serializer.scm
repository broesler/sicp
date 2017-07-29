;;==============================================================================
;;     File: serializer.scm
;;  Created: 04/07/2017, 14:12
;;   Author: Bernie Roesler
;;
;;  Description: Test serializer code from ch 3.4
;;
;;==============================================================================

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

;; from footnote -- MIT Scheme
; (define (test-and-set! cell)
;   (without-interrupts
;    (lambda ()
;      (if (car cell)
;          true
;          (begin (set-car! cell true)
;                 false)))))

;;==============================================================================
;;==============================================================================
