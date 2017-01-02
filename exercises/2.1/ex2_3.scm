;;==============================================================================
;;     File: ex2_3.scm
;;  Created: 11/08/2016, 22:42
;;   Author: Bernie Roesler
;;
;;  Description: Representation of rectangles in a plane
;;
;;==============================================================================
(load "ex2_2.scm") ; get make-segment, etc.

;------------------------------------------------------------------------------- 
;       Implementation-independent procedures
;-------------------------------------------------------------------------------
;; Perimeter of rectangle
(define (rect-perimeter rectangle)
  (let ((width (rect-width rectangle))
        (height (rect-height rectangle)))
    (+ (* 2 width) (* 2 height))))

;; Area of rectangle
(define (rect-area rectangle)
  (let ((width (rect-width rectangle))
        (height (rect-height rectangle)))
    (* width height)))

(define (test-rects r)
  (newline)
  (display ";    Perimeter = ")
  (display (rect-perimeter r)) ; Value: 4
  (newline)
  (display ";    Area = ")
  (display (rect-area r))) ; Value: 1

;------------------------------------------------------------------------------- 
;        Possible representations:
;-------------------------------------------------------------------------------
;;  x 4 points (need to check if they're a rectangle)
;;  - 2 corners (as points)
;;  - diagonal (as segment)
;;  - point and width + height (point and (two segments) or (two points))

;------------------------------------------------------------------------------- 
;        1. Represent as 2 corners
;-------------------------------------------------------------------------------
;; constructor
(define (make-rectangle corner1 corner2) 
  (cons corner1 corner2)) 

;; selectors (required for perimeter and area)
(define (rect-width rectangle)
  (let ((x1 (x-point (car rectangle)))
        (x2 (x-point (cdr rectangle))))
    (abs (- x1 x2))))

(define (rect-height rectangle)
  (let ((y1 (y-point (car rectangle)))
        (y2 (y-point (cdr rectangle))))
    (abs (- y1 y2))))

; Test code:
(display ";;; Two points:")
(define pt1 (make-point 0 0))
(define pt2 (make-point 1 1))
(define myrect (make-rectangle pt1 pt2))
(test-rects myrect)

;------------------------------------------------------------------------------- 
;        2. Represent as one corner and width + height
;-------------------------------------------------------------------------------
;; constructor
(define (make-rectangle point1 width height)
  (cons point1 (cons width height)))

;; selectors
(define (rect-width rectangle)
  (car (cdr rectangle)))

(define (rect-height rectangle)
  (cdr (cdr rectangle)))

; Test code:
(newline)
(display ";;; Point and width + height:")
(define pt1 (make-point 0 0))
(define myrect (make-rectangle pt1 1 1))
(test-rects myrect)
;;==============================================================================
;;==============================================================================
