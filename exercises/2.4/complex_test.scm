;;==============================================================================
;;     File: complex_test.scm
;;  Created: 12/14/2016, 22:31
;;   Author: Bernie Roesler
;;
;;  Description: Test complex variable procedures
;;
;;==============================================================================
;;; SECTION 2.4.2
(load "complex.scm")

;; Dummy these procedures to allow these tests to work without the generic
;; arithmetic system from Section 2.5:
(define add +)
(define mul *)
(define square-root sqrt)
(define sine sin)
(define cose cos)
(define arctan atan)

(define z1 (make-from-real-imag 1 1)) ;Value: z1

z1 ;Value 20: (rectangular 1 . 1)
(c-real-part z1) ;Value: 1
(c-imag-part z1) ;Value: 1
(c-magnitude z1) ;Value: 1.4142135623730951
(c-angle z1) ;Value: .7853981633974483

(define z2 (make-from-mag-ang 1.4142135623730951 .7853981633974483)) ;Value: z2

z2 ;Value 22: (polar 1.4142135623730951 . .7853981633974483)

(c-magnitude z2) ;Value: 1.4142135623730951
(c-angle z2) ;Value: .7853981633974483
(c-real-part z2) ;Value: 1.
(c-imag-part z2) ;Value: 1.

z1 ;Value 20: (rectangular 1 . 1)
z2 ;Value 22: (polar 1.4142135623730951 . .7853981633974483)

(add-complex z1 z2) ;Value 23: (rectangular 2. . 2.)
(sub-complex z1 z2) ;Value 24: (rectangular 0. . 0.)
(mul-complex z1 z2) ;Value 25: (polar 2.0000000000000004 . 1.5707963267948966)
(div-complex z1 z2) ;Value 26: (polar 1. . 0.)


;;==============================================================================
;;==============================================================================
