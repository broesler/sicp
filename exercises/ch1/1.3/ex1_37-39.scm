;;==============================================================================
;;     File: ex1_37.scm
;;  Created: 11/06/2016, 19:55
;;   Author: Bernie Roesler
;;
;;  Description: k-term continued fractions
;;
;;                      N1
;;          f = ---------------
;;              D1 +     N2
;;                  -----------
;;                   ... + Nk
;;                        ----
;;                         Dk 
;;
;;==============================================================================

; k-term continued fraction
; (a) linear recursive
; (define (cont-frac n d k)
;   (if (= k 1)
;     (/ (n k) (d k))
;     (/ (n k)
;        (+ (d k) 
;           (cont-frac n d (- k 1))))))

; (b) linear iterative
(define (cont-frac n d k)
  (define (iter result k)
    (if (= k 0)
      result
      (iter (/ (n k) 
               (+ (d k) result))
            (- k 1))))
  (iter 0.0 k))

;------------------------------------------------------------------------------- 
;        Ex 1.37
;-------------------------------------------------------------------------------
; Approximate 1/phi = 2 / (1 + sqrt(5)) = 0.6180339888 
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)

; k for 4 decimal-place accuracy: 
; k = 11, Value: 0.6180257510729613

;------------------------------------------------------------------------------- 
;        Ex 1.38
;-------------------------------------------------------------------------------
; Approximate (- e 2) based on Euler's expansion
;   e = 2.7182818285 - 2 = 0.7182818285
;   Ni = 1
;   Di = 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ... periodic every 3 terms
(cont-frac (lambda (i) 1.0)
           (lambda (i) 
             (if (= (remainder (+ i 1) 3) 0) ; 2nd number in period
               (* 2 (/ (+ i 1) 3))
               1.0))
           8)

;------------------------------------------------------------------------------- 
;        Ex 1.39
;-------------------------------------------------------------------------------
; Approximate tan x
; tan x = x / (1 - (x^2 / (3 - (x^2 / (5 - ...
(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1) 
                 x                      ; Ni = x    (first term)
                 (- (square x))))       ; Ni = -x^2 (every other term)
             (lambda (i) (- (* 2 i) 1)) ; Di = 1, 3, 5, 7, ...
             k))

; definition of pi
(define pi (* 4 (atan 1)))

; tan(pi/4) = 1.0
(tan-cf (/ pi 4) 9) ; Value: 1.0 (k = 6 gives 4 digits of precision)
;;==============================================================================
;;==============================================================================
