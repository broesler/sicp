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
; a procedure that takes an x-coordinate 
(define (vertical-line pt len)
  (lambda (t) 
    (make-point (x-of pt)           ; start at given x
                (+ (y-of pt)        ; y is point between original y
                   (* t len)))))    ;   and y+len (t on [0,1])

; 3. vertical-line is type: (Point, Sch-Num) -> Curve

;------------------------------------------------------------------------------- 
;        Ex 3
;-------------------------------------------------------------------------------
(define (reflect-through-y-axis curve)
  (lambda (t)
    (let ((ct (curve t)))
      (make-point (x-of ct) (- (y-of ct))))))

;------------------------------------------------------------------------------- 
;        Ex 4
;-------------------------------------------------------------------------------
; connect-ends returns a Curve that is a copy of curve1 followed by a copy of
; curve2 after it has been rigidly translated so its starting point coincides
; with the end point of curve1.
(define (connect-ends curve1 curve2)
  (let* ((curve1-copy curve1)
         (curve2-copy curve2)
         (end-pt1 (curve1-copy 1))
         (start-pt2 (curve2-copy 0))
         (curve2-translated-to-curve1
           ((translate (- (x-of start-pt2) (x-of end-pt1))
                       (- (y-of start-pt2) (y-of end-pt1)))
            curve2-copy)))
    curve2-translated-to-curve1))

;------------------------------------------------------------------------------- 
;        Ex 5
;-------------------------------------------------------------------------------
;; Create graphics window
(define g1 (make-graphics-device (car (enumerate-graphics-types))))
; (define g2 (make-graphics-device (car (enumerate-graphics-types))))
; (define g3 (make-graphics-device (car (enumerate-graphics-types))))

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
   ((squeeze-rectangular-portion -.5 1.5 -.5 1.5)
    (gosper-curve-arbitrary level initial-curve))))

; Test code: (these two should be the same, but one is unconnected)
; (show-connected-gosper 5)
; (show-points-gosper g1 5 200 unit-line)
;;==============================================================================
;;==============================================================================
