;;==============================================================================
;;     File: ex3_51.scm
;;  Created: 08/13/2017, 22:31
;;   Author: Bernie Roesler
;;
;;  Description: Delayed evaluation printing?
;;
;;==============================================================================
(load "streams.scm")

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)
; Value: 0 1 2 3 4 5 6 7 ;... done ;Value: 7

;; The "pointer" x gets moved along the stream when we call stream-ref, so the
;; first call prints 0:5, then the next call only prints 6:7.
;;==============================================================================
;;==============================================================================
