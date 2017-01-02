;;==============================================================================
;;     File: ex2_1.scm
;;  Created: 11/08/2016, 21:45
;;   Author: Bernie Roesler
;;
;;  Description: make-rat improved
;;
;;==============================================================================
(load "rationals.scm") ; load rationals procedures

; old definition:
; (define (make-rat n d) 
;   (let ((g (gcd n d)))
;     (cons (/ n g) (/ d g))))

; Redefine make-rat to improve it
; Cases: n > 0  d > 0 ==> n/d > 0
;        n < 0  d < 0 ==> n/d > 0
;        n > 0  d < 0 ==> n/d < 0 (via n < 0)
;        n < 0  d > 0 ==> n/d < 0 (via n < 0)
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((or (and (> n 0) (> d 0)) 
               (and (< n 0) (< d 0))) 
           (cons (/ (abs n) g) (/ (abs d) g)))
          ((or (and (> n 0) (< d 0)) 
               (and (< n 0) (> d 0))) 
           (cons (/ (- (abs n)) g) (/ (abs d) g))))))

; Test code:
(define (test-ex2_1)
  (define minus-one-half (make-rat 1 (- 2)))
  (define one-third (make-rat (- 1) 3))
  (print-rat minus-one-half)
  (print-rat (add-rat minus-one-half one-third)))
; (test-ex2_1)
;;==============================================================================
;;==============================================================================
