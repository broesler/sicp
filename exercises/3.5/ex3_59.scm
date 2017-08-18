;;==============================================================================
;;     File: ex3_59.scm
;;  Created: 08/18/2017, 12:00
;;   Author: Bernie Roesler
;;
;;  Description: Power series
;;
;;==============================================================================
(load "streams.scm")

;;; (a) integrate a power series s = a0, a1, a2, a3, ...
;;; to return int(s) = 1/1*a0, 1/2*a1, 1/3*a2, 1/4*a3, ...
(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (integrate-series s)
  (div-streams s integers))

(display "∫ 1 + x + x^2 + ... dx = ")
(define s (cons-stream 0 (integrate-series ones)))
(display-stream-n s 5)

;;; (b) Define exponential series as its own derivative, except for e^0 = 1
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

;;; Generate sin and cos series from exponential
(define (negate x) (- x))

(define cosine-series
  (cons-stream 1 (integrate-series (stream-map negate sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;;; Test code:
(display "e^x = ")
(display-stream-n exp-series 5)
(display "Cosine = ")
(display-stream-n cosine-series 5)
(display "Sine = ")
(display-stream-n   sine-series 5)
;;==============================================================================
;;==============================================================================
