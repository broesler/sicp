;;==============================================================================
;;     File: ex2_2.scm
;;  Created: 11/08/2016, 22:22
;;   Author: Bernie Roesler
;;
;;  Description: Constructing line segments 
;;
;;==============================================================================

;; segment constructor 
(define (make-segment start-point end-point)
  (cons start-point end-point))

;; end-point selectors
(define (start-segment segment) (car segment))
(define (end-segment   segment) (cdr segment))

;; point constructor
(define (make-point x y) (cons x y))

;; point selectors
(define (x-point point) (car point))
(define (y-point point) (cdr point))

;; find midpoint of segment
(define (midpoint-segment segment)
  ;; average coordinates
  (define (average a b) 
    (/ (+ a b) 2.0))
  (let* ((p1 (start-segment segment))
         (p2 (end-segment segment))
         (xm (average (x-point p1) (x-point p2)))
         (ym (average (y-point p1) (y-point p2))))
    (make-point xm ym)))

;; pretty-printing
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; Test code:
(define (test-ex2_2)
  (define origin (make-point 0 0))
  (define p1 (make-point 1 1))
  (define seg (make-segment origin p1))
  (print-point (midpoint-segment seg)))
; (test-ex2_2)
;;==============================================================================
;;==============================================================================
