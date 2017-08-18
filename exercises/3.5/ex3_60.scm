;;==============================================================================
;;     File: ex3_60.scm
;;  Created: 08/18/2017, 12:24
;;   Author: Bernie Roesler
;;
;;  Description: Multiply power series
;;
;;==============================================================================
(load "streams.scm")

(define (add-series s1 s2)
  (add-streams s1 s2))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (scale-stream (stream-cdr s1)
                                          (stream-car s2))
                            (mul-series s1 (stream-cdr s2)))))

;;; Verify that sin^2 x + cos^2 x = 1
(load "ex3_59.scm") ; also displays test code from ex3.59
(define cos2 (mul-series cosine-series
                         cosine-series))
(define sin2 (mul-series sine-series
                         sine-series))

; (display-stream-n (add-series cos2 sin2) 5)

; Value: 1
; Value: 0
; Value: 0
; Value: 0
; Value: 0

;;==============================================================================
;;==============================================================================
