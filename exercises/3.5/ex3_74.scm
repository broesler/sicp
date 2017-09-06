;;==============================================================================
;;     File: ex3_74.scm
;;  Created: 09/04/2017, 19:17
;;   Author: Bernie Roesler
;;
;;  Description: zero-crossings
;;
;;==============================================================================
(load "streams.scm")

(define sense-data 
 (list-stream (list 1.0 2.0 1.5 1.0 0.5 -0.1 -2.0 -3.0 -2.0 -0.5 0.2 3.0 4.0)))

;; 1 if positive or 0, 0 if negative
(define (sign x)
  (if (or (zero? x) (positive? x))
    1
    0))

;; a is next value, b is previous value
;;  1 if a +, b -  (b -> a is positive slope)
;; -1 if a -, b +  (b -> a is negative slope)
;;  0 if a == b
(define (sign-change-detector a b)
  (cond ((> (sign a) (sign b))  1)
        ((< (sign a) (sign b)) -1)
        (else 0)))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

; (define zero-crossings (make-zero-crossings sense-data 0))

;; Alternate definition
(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

(display-stream-n zero-crossings 12)

;------------------------------------------------------------------------------- 
;        Ex 3.75
;-------------------------------------------------------------------------------
;; Smooth sense-data before checking for zero-crossings
(define (make-zero-crossings input-stream last-value last-avg)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avg)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))

(define smoothed-zero-crossings 
  (make-zero-crossings sense-data 0 0))

;------------------------------------------------------------------------------- 
;        Ex 3.76
;-------------------------------------------------------------------------------
(define (smooth s)
  (stream-map (lambda (x y) 
                (/ (+ x y) 2)) 
              s
              (cons-stream 0 s)))

(define zero-crossings 
  (let ((smoothed (smooth sense-data)))
    (stream-map sign-change-detector smoothed (cons-stream 0 smoothed))))

(display-stream-n zero-crossings 13)

;;==============================================================================
;;==============================================================================
