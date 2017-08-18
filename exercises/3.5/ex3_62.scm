;;==============================================================================
;;     File: ex3_62.scm
;;  Created: 08/18/2017, 13:23
;;   Author: Bernie Roesler
;;
;;  Description: divide two power series
;;
;;==============================================================================
(load "ex3_61.scm")

;; We need to invert a *unit* series, so scale the denominator by 1/d0, then
;; rescale the entire quotient by d0 to cancel out the effect
(define (div-series n d)
  (let ((d0 (stream-car d)))
    (scale-stream 
      (mul-series n 
                (invert-unit-series (scale-stream d (/ 1 d0))))
      d0)))

(define tan-series (div-series sine-series cosine-series))
(display-stream-n tan-series 6)

; Value: 0
; Value: 1
; Value: 0
; Value: 1/3
; Value: 0
; Value: 2/15
;;==============================================================================
;;==============================================================================
