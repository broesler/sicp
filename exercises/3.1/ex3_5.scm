;;==============================================================================
;;     File: ex3_5.scm
;;  Created: 01/11/2017, 11:25
;;   Author: Bernie Roesler
;;
;;  Description: Monte Carlo integration
;;
;;==============================================================================

;;; Consider computing the area of a region of space described by a predicate
;;; P(x,y) that is true for points (x,y) in the region and false for points not
;;; in the region. 
;;;   1. Pick random point (x,y) in rectangle
;;;   2. Test P(x,y) to determine if point is in region
;;;   3. Fraction of points that fall in region * area of rectangle = integral

;;; Arguments: predicate, upper and lower bounds of rectangle, number of trials
;;; Assume: x2 > x1, y2 > y1
(define (estimate-integral P x1 x2 y1 y2 n-trials)
  (let ((x (lambda () (random-in-range x1 x2)))
        (y (lambda () (random-in-range y1 y2))))
    (let ((experiment (lambda () (P x y))))
      (let ((frac (monte-carlo n-trials experiment))
            (area (* (- x2 x1) (- y2 y1))))
        (* frac area)))))

;;; Monte Carlo from Section 3.1.2 (ch3.scm)
;;; Returns the fraction of trials that passed the experiment
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;;; Return a random number in a range [low,high)
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

;;; Area of circle radius 1 (== pi)
;;; Arguments are procedures that produce numbers
(define (circle-test x y)
  (let ((xc 0.0)
        (yc 0.0)
        (r  1.0))
  (<= (+ (square (- (x) xc)) 
         (square (- (y) yc))) 
      (square r))))

;;; Test code:
(printval (estimate-integral circle-test -1.0 1.0 -1.0 1.0 1e4))
; n-trials = 1e4 gives 2-digit precision:
; Value: 3.1428
; Value: 3.1388
; Value: 3.16
; Value: 3.1412
; Value: 3.1696

;;==============================================================================
;;==============================================================================
