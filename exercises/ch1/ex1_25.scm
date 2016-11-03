;;==============================================================================
;;     File: ex1_25.scm
;;  Created: 11/03/2016, 18:38
;;   Author: Bernie Roesler
;;
;;  Description: 
;;
;;==============================================================================
(load "ex1_16.scm") ; for fast-expt
(load "ex1_22.scm") ; for expmod

; print outputs of square as it is called
(define (square m)
  (newline)
  (display "square ")
  (display m)
  (* m m))

; Test differences by multiplying large numbers
(display "Custom procedure:   (expmod 5 101 101)")
(define value (expmod 5 101 101))
(display "\n; Value: ")
(display value)
(newline)
(newline)

(display "Proposed procedure: (remainder (fast-expt 5 101) 101)")
(define value (remainder (fast-expt 5 101) 101))
(display "\n; Value: ")
(display value)

;------------------------------------------------------------------------------- 
;        Output
;-------------------------------------------------------------------------------
; ;Loading "ex1_25.scm"...
; ;  Loading "ex1_16.scm"... done
; ;  Loading "ex1_22.scm"... done
; Custom procedure:   (expmod 5 101 101)
; square 5
; square 24
; square 71
; square 92
; square 1
; square 1
; ; Value: 5
;
; Proposed procedure: (remainder (fast-expt 5 101) 101)
; square 5
; square 125
; square 15625
; square 244140625
; square 298023223876953125
; square 88817841970012523233890533447265625
; ; Value: 5
; ;... done
; ;Unspecified return value

;;==============================================================================
;;==============================================================================
