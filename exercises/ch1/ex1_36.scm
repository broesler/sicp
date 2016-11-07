;;==============================================================================
;;     File: ex1_36.scm
;;  Created: 11/06/2016, 19:55
;;   Author: Bernie Roesler
;;
;;  Description: Continued fractions
;;
;;                      N1
;;          f = -------------------
;;              D1 +       N2
;;                  ---------------
;;                   D2 +     N3
;;                        ---------
;;                         D3 + ...
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
  (define (iter result n d k)
    (if (= k 0)
      result
      (iter (/ (n k) 
               (+ (d k) result))
            n
            d
            (- k 1))))
  (iter 1.0 n d k))

; Test Code:
; Approximate 1/phi = 2 / (1 + sqrt(5)) = 0.6180339888 
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           12)

; k for 4 decimal-place accuracy: 
; k = 12, Value: 0.6180257510729613

;;==============================================================================
;;==============================================================================
