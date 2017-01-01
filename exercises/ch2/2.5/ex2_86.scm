;;==============================================================================
;;     File: ex2_86.scm
;;  Created: 01/01/2017, 14:14
;;   Author: Bernie Roesler
;;
;;  Description: Complex numbers with generic parts
;;
;;==============================================================================
(load "ex2_85.scm")

;;; NOTE: see updates in "generic_arithmetic.scm"
;;; + - * / ==> add sub mul div
(define aa (make-complex-from-real-imag a a))
(define ac (make-complex-from-mag-ang a (div c 4)))
(define bb (make-complex-from-real-imag b (make-rational 2 5)))
(printval (add aa aa)) ; Value: (complex rectangular 14 . 14)
(printval (add aa ac)) ; Value: (complex rectangular 11.949750751954584 . 11.949744184654904)
;; These lines fail:
;; Need to redefine "project" for a complex number... could now contain parts
;; that are various types, so we have to recursively project?
(printval (add aa bb))
(printval (add bb bb))
;;==============================================================================
;;==============================================================================
