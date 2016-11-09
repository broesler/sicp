;;==============================================================================
;;     File: sigma.scm
;;  Created: 10/28/2016, 14:40
;;   Author: Bernie Roesler
;;
;;  Description: sigma notation
;;==============================================================================

; Sum up integers
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

; sum up cubes
(define (cube x)
  (* x x x))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

; sum up to pi/8
(define (pi-sums a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

;------------------------------------------------------------------------------- 
;       Create general summation notation 
;-------------------------------------------------------------------------------
; sum <term> from i = <a> to <b>, where <a> is updated by <next>
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a) 
       (sum term (next a) next b))))

; Could also do iterative implementation (from lecture 2A):
; (define (sum term a next b)
;   (define (iter j ans)
;     (if (> j b)
;       ans
;       (iter (next j)
;             (+ (term j) ans))))
;   (iter a 0))

; sum integers
(define (sum-int a b)
  (define (identity a) a)
  (sum identity a 1+ b))

; sum cubes
(define (sum-cube a b)
  (sum cube a 1+ b))

; pi sum
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 ( * x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

; or, use lambda functions
; (define (pi-sum a b)
;   (sum (lambda (i) (/ 1.0 (* i (+ i 2.0))))
;        a
;        (lambda (i) (+ i 4.0))
;        b))

; Test code
(sum-integers 1 10)     ;Value: 55
(sum-int 1 10)          ;Value: 55
(sum-cubes 1 10)        ;Value: 3025
(sum-cube 1 10)         ;Value: 3025
(* 8 (pi-sums 1 1000))  ;Value: 3.139592655589783
(* 8 (pi-sum 1 1000))   ;Value: 3.139592655589783

;------------------------------------------------------------------------------- 
;        Integral
;-------------------------------------------------------------------------------
; General integral forumula:
; \int_a^b f(x) = [f(a + dx/2) + f(a + dx + dx/2) + f(a + 2*dx + dx/2) + ...]*dx
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; Test code (\int_0^1 x^3 dx = 1/4)
(integral cube 0 1 0.01)  ;Value: .24998750000000042
(integral cube 0 1 0.001) ;Value: .249999875000001
;;==============================================================================
;;==============================================================================
