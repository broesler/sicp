;;==============================================================================
;;     File: ex1_31-33.scm
;;  Created: 11/04/2016, 12:48
;;   Author: Bernie Roesler
;;
;;  Description: General product procedure analogous to sum
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
      (iter (next a) (+ (term a) result))))
  (iter a 0.0))

;------------------------------------------------------------------------------- 
;        Product
;-------------------------------------------------------------------------------
; Pi notation:
; multiply <term> from i = <a> to <b>, where <a> is updated by <next>
; (a) Linear recursion
(define (product term a next b)
  (if (> a b)
    1
    (* (term a) 
       (product term (next a) next b))))

; (b) Linear iteration
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1.0))

; Write factorial in terms of product
(define (fact n)
  (define (identity x) x)
  (product identity 1 1+ n))

; Test code:
; (fact 5) ;Value: 120

; Compute approximations to pi using:
;  pi    2 * 4 * 4 * 6 * 6 * 8 ...
;  -- = ---------------------------
;  4     3 * 3 * 5 * 5 * 7 * 7 ...
(define (pi-frac n)
  (define (pi-term k) 
    (/ (* (* 2.0 k) (+ (* 2.0 k) 2.0)) 
       (square (+ (* 2.0 k) 1.0))))
  (product pi-term 1 1+ n))

; Test code:
; (* 4.0 (pi-frac 1000)) 
;Value: 3.142377365093882  (1e3 terms)
;Value: 3.1415934389872975 (1e6 terms) only works with iteration!

;;==============================================================================
;;==============================================================================
