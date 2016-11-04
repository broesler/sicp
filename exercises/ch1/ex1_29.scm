;;==============================================================================
;;     File: ex1_29.scm
;;  Created: 11/03/2016, 20:47
;;   Author: Bernie Roesler
;;
;;  Description: Simpson's Rule implementation 
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

; Simpson's rule O(h^2)
; \int_a^b f(x) dx = 
;   h/3 * [y0 + 2*y1 + 4*y2 + 2*y3 + 4*y4 + ... + 2*yn-2 + 4*yn-1 + yn ]
;   where h = (b - a)/3
;     and yk = f(a + kh)
; TODO how to do weighting of f(x)?? 
;   Need to pass (sum) a procedure that takes one argument (a), but produces the
;   proper weighting of each term (function of k?)
(define (simpson f a b n)
  (let ((h (/ (- b a) n)))
    (define (g k)
      (cond ((or (= k 0) (= k n)) (f (+ a (* k h))))
            ((= (remainder k 2) 0) (* 2.0 (f (+ a (* k h)))))
            ((= (remainder k 2) 1) (* 4.0 (f (+ a (* k h)))))))
  (* (/ h 3.0)
     (sum g 0 1+ n))))

;------------------------------------------------------------------------------- 
;       Test code vs integral formula 
;-------------------------------------------------------------------------------
; Midpoint formula O(h):
; \int_a^b f(x) dx 
;   = [f(a + dx/2) + f(a + dx + dx/2) + f(a + 2*dx + dx/2) + ...]*dx
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; Pretty-print
(define (print-result x)
  (newline)
  (display "integral = ")
  (display x))

; Test code (\int_0^1 x^3 dx = 1/4)
(print-result (integral cube 0.0 1.0 0.01))
(print-result (integral cube 0.0 1.0 0.001))
(print-result (simpson  cube 0.0 1.0 100))
(print-result (simpson  cube 0.0 1.0 1000))

; Output:
; ;Loading "ex1_29.scm"...
; integral = .24998750000000042
; integral = .24999987500000073
; integral = .25000000000000006
; integral = .25000000000000006
; ;... done
; ;Unspecified return value

;;==============================================================================
;;==============================================================================
