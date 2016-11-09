;;==============================================================================
;;     File: ps2_1.scm
;;  Created: 11/07/2016, 21:21
;;   Author: Bernie Roesler
;;
;;  Description: Exercise 1 from PS 2 - Graphing with Higher Order Procedures
;;
;;==============================================================================
; Load problem set code
(load "curves.scm")
(load "drawing.scm")
(load "utils.scm")

; Create graphics window
(graphics-clear g1)
; (define g1 (make-graphics-device (car (enumerate-graphics-types))))
; (define g2 (make-graphics-device (car (enumerate-graphics-types))))
; (define g3 (make-graphics-device (car (enumerate-graphics-types))))

;------------------------------------------------------------------------------- 
;       Ex 1 
;-------------------------------------------------------------------------------
; For what value of n will 
;   (((thrice thrice) f) 0) 
; return the same value as
;   ((repeated f n) 0)?
;
; n = 27 (thrice => 3^3^3^...)
; Test code:
(define (f x) (+ 1 x))
(((thrice thrice) f) 0)
((repeated f 27) 0)

; Test code:
; 1.
(((thrice thrice) 1+) 6)
; Value: 33
; Increments 6 3^3 times, returns a Sch-Num

; 2.
(((thrice thrice) identity) compose)
; Value: #[compound-procedure]
; Returns 27 nested 'identity' calls with compose as argument. Result is
; a compound procedure of type (F,F) -> F (identical to compose itself).

; 3.
(((thrice thrice) square) 1)
; Value: 1
; Multiplies 1 by itself 27 times

; 4.
; (display (((thrice thrice) square) 2))
; Value: #[inf]??
; Raises 2^2^2^... 27 times. Breaks! Does not return in a reasonable amt of time

;------------------------------------------------------------------------------- 
;       Ex 2 
;-------------------------------------------------------------------------------
; 1. What is the type of unit-line-at?
; unit-line-at is type: 
;   Sch-Num -> Curve
; it takes a number, and returns a procedure that takes a numer and returns
; a Point

; Construct a unit line parallel to the x-axis at given y point
(define (unit-line-at y)
  (lambda (t) (make-point t y)))

; 2. define vertical-line by procedure that takes a point and a length, returns
; a procedure that takes the curve parameter t
(define (vertical-line pt len)
  (lambda (t) 
    (make-point (x-of pt)           ; start at given x
                (+ (y-of pt)        ; y is point between original y
                   (* t len)))))    ;   and y+len (t on [0,1])

; 3. vertical-line is type: (Point, Sch-Num) -> Curve

; Test code:
; (define my-point (make-point 0 0))
; (define vline (vertical-line my-point 0.5))
; ((draw-connected g1 200) vline)

;------------------------------------------------------------------------------- 
;        Ex 3
;-------------------------------------------------------------------------------
; f(x) -> f(-x)
(define (reflect-through-y-axis curve)
  (lambda (t)
    (let ((ct (curve t)))
      (make-point (- (x-of ct)) (y-of ct)))))

; Test code:
(define (sine-curve t) 
  (make-point t (sin (* 2pi t))))
; ((draw-connected g1 200) sine-curve)
; ((draw-connected g1 200) (reflect-through-y-axis sine-curve))

;------------------------------------------------------------------------------- 
;        Ex 4
;-------------------------------------------------------------------------------
; connect-ends returns a Curve that is a copy of curve1 followed by a copy of
; curve2 after it has been rigidly translated so its starting point coincides
; with the end point of curve1.
(define (connect-ends curve1 curve2)
  (let* ((start2 (curve2 0))
         (end1 (curve1 1))
         (transop (translate (- (x-of end1) (x-of start2))
                             (- (y-of end1) (y-of start2)))))
    (connect-rigidly curve1 (transop curve2))))

(define two-lines (connect-ends unit-line sine-curve))
; ((draw-points-on g1 200) ((squeeze-rectangular-portion 0 2 0 1) two-lines))

;------------------------------------------------------------------------------- 
;        Ex 5
;-------------------------------------------------------------------------------
;; Draw unit circles (comment out for now)
; ((draw-connected g1 200) unit-circle)
; ((draw-connected g2 200) alternative-unit-circle)
; ((draw-points-on g3 200) unit-circle)
; ((draw-points-squeezed-to-window g3 200) unit-circle)

; (graphics-close g1)
; (graphics-close g2)
; (graphics-close g3)

;------------------------------------------------------------------------------- 
;        Ex 6
;-------------------------------------------------------------------------------
(define (gosper-curve-arbitrary level curve)
  ((repeated gosperize level) curve))

(define (show-points-gosper window level number-of-points initial-curve)
  ((draw-points-on window number-of-points)
   ; ((squeeze-rectangular-portion -.5 1.5 -.5 1.5)
   ((squeeze-rectangular-portion 0.5 1.5 0.5 1.5)
    (gosper-curve-arbitrary level initial-curve))))

(define (show-connected-gosper window level number-of-points initial-curve)
  ((draw-connected window number-of-points)
   ;; ((squeeze-rectangular-portion xlo xhi ylo yhi)
   ((squeeze-rectangular-portion 0.5 1.5 0.5 1.5)
    (gosper-curve-arbitrary level initial-curve))))

; Test code: (these two should be the same, but one is unconnected)
; (show-connected-gosper g1 5 200 unit-line)
; (show-points-gosper g1 5 200 unit-line)

; Gosperize the arc of the unit circle from 0 to pi (t = [0,0.5])
(define (semi-circle t)
  (make-point (sin (* pi t))
              (cos (* pi t))))

; Test semi-circle:
; ((draw-connected g1 200) semi-circle)

;; Gosperize
; (show-connected-gosper g1 10 100000 semi-circle)
; (show-connected-gosper g1 10 100000 unit-line)

;------------------------------------------------------------------------------- 
;        Ex 7
;-------------------------------------------------------------------------------
; ordinary Gosper curve with param-gosper
; ((draw-connected-squeezed-to-window g1 200) 
;  (param-gosper 5 (lambda (level) pi/4)))

; Original using lots of trig
(define (param-gosper level angle-at)
  (if (= level 0)
      unit-line
      ((param-gosperize (angle-at level))
       (param-gosper (- level 1) angle-at))))

; Steps:
; 1. scale curve
; 2. connect-rigidly 
;   (a) curve rotated about origin by theta
;   (b) curve rotated about origin by -theta, then translated to (a) end point
(define (param-gosperize theta)
  (lambda (curve)
    (let ((scale-factor (/ (/ 1 (cos theta)) 2)))
      (let ((scaled-curve ((scale scale-factor) curve)))
        (connect-rigidly ((rotate-around-origin theta) scaled-curve)
                         ((translate .5 (* (sin theta) scale-factor))
                          ((rotate-around-origin (- theta)) scaled-curve)))))))

; My cleaner version
(define (my-param-gosper level angle-at)
  (if (= level 0)
      unit-line
      ((my-param-gosperize (angle-at level))
       (my-param-gosper (- level 1) angle-at))))

; (A) Rewrite param-gosperize, but without all the math
(define (my-param-gosperize theta)
  (lambda (curve)
    (let ((scale-factor (/ (/ 1 (cos theta)) 2)))
      (let ((scaled-curve ((scale scale-factor) curve)))
        (put-in-standard-position
          (connect-ends ((rotate-around-origin theta) scaled-curve)
                        ((rotate-around-origin (- theta)) scaled-curve)))))))

; (B) Make curves with these angles:
(define (angle-at-1 level)
  (/ pi (+ level 2)))

(define (angle-at-2 level)
  (/ pi (expt 1.3 level)))

; Draw!
; ((draw-connected-squeezed-to-window g1 10000) (param-gosper 5 angle-at-1))
; ((draw-connected-squeezed-to-window g1 10000) (param-gosper 5 angle-at-2))

; (C) Compare timing among 'gosper-curve', 'param-gosper', and 'my-param-gosper'
(show-time (lambda () ((   gosper-curve 100) .1)))
(show-time (lambda () ((   param-gosper 100 (lambda (level) pi/4)) .1)))
(show-time (lambda () ((my-param-gosper 100 (lambda (level) pi/4)) .1)))
;; Output:
;;  process time: 0 (0 RUN + 0 GC); real time: 1
;;  process time: 0 (0 RUN + 0 GC); real time: 1
;;  process time: 260 (260 RUN + 0 GC); real time: 268

;------------------------------------------------------------------------------- 
;        Ex 8 timing
;-------------------------------------------------------------------------------
; (A) Is Ben's definition correct? yes, but it's slow...
(define (bens-rotate theta)
  (let ((cth (cos theta))
        (sth (sin theta)))
    (lambda (curve)
      (lambda (t)
        (let ((x (x-of (curve t)))  ;Ben writes (curve t)
              (y (y-of (curve t)))) ;twice
          (make-point
              (- (* cth x) (* sth y))
              (+ (* sth x) (* cth y))))))))

; (B) trace x-of
(trace-entry x-of)

(define (test-gosper n)
  (newline)
  (display "n = ")
  (display n)
  ((gosper-curve n) .1))

; Try original rotate for 3 test runs
(newline)
(display ";;;;;;;;;; Testing original rotate:")
(test-gosper 1) ; x-of count:  2
(test-gosper 2) ; x-of count:  4
(test-gosper 5) ; x-of count: 12

; Now test Ben's rotate procedure
(define temp-rotate rotate-around-origin) ; store original in temp
(define rotate-around-origin bens-rotate)
(newline)
(display ";;;;;;;;;; Testing Ben's rotate:")
(test-gosper 1) ; x-of count:   3
(test-gosper 2) ; x-of count:   9
(test-gosper 5) ; x-of count: 117 
;;==============================================================================
;;==============================================================================
