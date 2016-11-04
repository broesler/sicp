;;==============================================================================
;;     File: ex1_31-33.scm
;;  Created: 11/04/2016, 12:48
;;   Author: Bernie Roesler
;;
;;  Description: Generalizing the sum procedure to accumulate and filter
;;
;;==============================================================================

; Sigma notation:
; sum <term> from i = <a> to <b>, where <a> is updated by <next>
; Linear recursion
; (define (sum term a next b)
;   (if (> a b)
;     0
;     (+ (term a) 
;        (sum term (next a) next b))))

; Linear iteration
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a)
            (+ (term a) result))))
  (iter a 0.0))

;------------------------------------------------------------------------------- 
;        Product
;-------------------------------------------------------------------------------
; Pi notation:
; multiply <term> from i = <a> to <b>, where <a> is updated by <next>
; Linear recursion
(define (product term a next b)
  (if (> a b)
    1
    (* (term a) 
       (product term (next a) next b))))

; Write factorial in terms of product
(define (fact n))

; Compute approximations to pi using:
;  pi    2 * 4 * 4 * 6 * 6 * 8 ...
;  -- = ---------------------------
;  4     3 * 3 * 5 * 5 * 7 * 7 ...
(define (pi-frac n))
;;==============================================================================
;;==============================================================================
