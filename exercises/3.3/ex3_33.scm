;;==============================================================================
;;     File: ex3_33.scm
;;  Created: 02/01/2017, 22:19
;;   Author: Bernie Roesler
;;
;;  Description: Constraint averager
;;
;;==============================================================================
(load "constraints.scm")

;;; Define averagers s.t. c = (a+b)/2
(define (averager a b c)
  (let ((u (make-connector))
        (v (make-connector)))
    (adder a b u)
    (multiplier u v c)
    (constant 0.5 v)
    'ok))

;;; Test code:
(define a (make-connector))
(define b (make-connector))
(define c (make-connector))
(probe "a" a)
(probe "b" b)
(probe "c" c)
(averager a b c)
(set-value! a 1.0 'bernie)
; Value: Probe: a = 1.
(set-value! b 3.0 'bernie)
; Value: Probe: c = 2.
; Value: Probe: b = 3.
(forget-value! b 'bernie)
; Value: Probe: c = ?
; Value: Probe: b = ?
(set-value! c 4.0 'bernie)
; Value: Probe: b = 7.
; Value: Probe: c = 4.
;;==============================================================================
;;==============================================================================
