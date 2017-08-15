;;==============================================================================
;;     File: ex3_52.scm
;;  Created: 08/14/2017, 21:31
;;   Author: Bernie Roesler
;;
;;  Description: Delayed evaluation test
;;
;;==============================================================================
(load "streams.scm")

;; Define delay WITHOUT memoproc --> same results!
; (define (delay x) 
;   (lambda () 
;     x))

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(printval sum) ; Value: 1 (only first element called upon in definition

(define y (stream-filter even? seq))
(printval sum) 
; Value: 6 (pulls through first element of y = 1+2+3 = 6, but 1 already added,
; so add 2 and 3 so sum)

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(printval sum)
; Value: 10 (pulls through first element of z = 1+2+3+4 = 10, but first
; 3 elements already calculated and memoized, so only add 4)

;; Given:
(printval (stream-ref y 7)) ; Value: 136 (add values through y[7])
(printval sum) ; Value: 136

(display-stream z)
(printval sum) ; Value: 210 (add values through z[end])

;; int = 1 2 3  4  5  6  7  8  9 10 11 12 13  14  15  16  17  18  19  20
;; seq = 1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210
;;   y =     0  1        2  3        4  5          6   7           8   9
;;   z =     0  1              2  3            4   5               6   7

;;==============================================================================
;;==============================================================================
