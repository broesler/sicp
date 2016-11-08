;;==============================================================================
;;     File: ex1_42.scm
;;  Created: 11/07/2016, 18:18
;;   Author: Bernie Roesler
;;
;;  Description:
;;
;;==============================================================================

;------------------------------------------------------------------------------- 
;       Ex 1.42: Composition of functions 
;-------------------------------------------------------------------------------
(define (compose f g)
  (lambda (x) (f (g x))))

; Test code:
(define (inc x) (+ x 1))
((compose square inc) 6) ; Value: 49

;------------------------------------------------------------------------------- 
;        Ex 1.43: repeated application of functions
;-------------------------------------------------------------------------------
(define (repeated f n)
  (if (= n 1)
    (lambda (x) (f x))
    (repeated (compose f f) (- n 1))))

; Test code:
((repeated square 2) 5) ; Value: 625

;------------------------------------------------------------------------------- 
;       Ex 1.44: n-fold smoothing 
;-------------------------------------------------------------------------------
(define (smooth f)
  (lambda (x) (/ (+ (f (+ x dx)) 
                    (f x) 
                    (f (- x dx))) 
                 3.0)))

; Test code:
(define (f x) (square x))
(define dx 0.001)
(define n 3)
(define x 1.0)

; general n-fold smoothing of f(x)
((repeated (smooth f) n) x) ; Value: 1.000010000031554

;------------------------------------------------------------------------------- 
;       Ex 1.45: nth roots 
;-------------------------------------------------------------------------------
; Find a fixed-point of f
(define (fixed-point f start)
  (define tol 0.0001)
  (define (close-enough? u v)
    (< (abs (- u v)) tol))
  (define (iter old new)
    (if (close-enough? old new)
      new
      (iter new (f new))))
  (iter start (f start)))

; NOTE "average-damp" is a procedure which takes a procedure as its argument,
; and returns a procedure which takes one argument
(define (average x y)
  (/ (+ x y) 2.0))

(define (average-damp f)
  (lambda (x) (average (f x) x)))

; Single procedure for computing nth roots
(define (nth-root x n)
  (define f (lambda (y) (/ x (expt y (- n 1)))))
  (fixed-point ((repeated average-damp 2) f) 1.0))

; Test code: (2 damps)
(nth-root  2.0 2.0) ; Value: 1.414142708876616
(nth-root  8.0 3.0) ; Value: 2.000008653640297
(nth-root 81.0 4.0) ; Value: 3.000000000000033

;;==============================================================================
;;==============================================================================
