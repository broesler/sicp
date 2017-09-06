;;==============================================================================
;;     File: ex3_73.scm
;;  Created: 09/04/2017, 18:53
;;   Author: Bernie Roesler
;;
;;  Description: RC-circuit simulator
;;
;;==============================================================================
(load "streams.scm")

;; General integral procedure
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R)
                 (scale-stream (integral i v0 dt) 
                               (/ 1 C)))))

(define RC1 (RC 5 1 0.5))

; (display-stream-n (RC1 ones 0) 10)
;;==============================================================================
;;==============================================================================
