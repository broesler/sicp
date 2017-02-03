;;==============================================================================
;;     File: ex3_37.scm
;;  Created: 02/03/2017, 13:34
;;   Author: Bernie Roesler
;;
;;  Description: expression-oriented celcius-fahrenheit converter
;;
;;==============================================================================
(load "constraints.scm")

;;; Constraint verions of aritmetic operators
;; x + y = z
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

;; x = z - y
(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

;; x * y = z
(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

;; x = z / y
(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

;; x = z
(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

;;; From book:
(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

;;; Test code:
(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)
(set-value! C 25 'user)
; Probe: Celsius temp = 25
; Probe: Fahrenheit temp = 77
; (set-value! F 212 'user) 
; Contradiction (77 212)
(forget-value! C 'user)
; Probe: Celsius temp = ?
; Probe: Fahrenheit temp = ?
(set-value! F 212 'user)
; Probe: Fahrenheit temp = 212
; Probe: Celsius temp = 100

;;==============================================================================
;;==============================================================================
