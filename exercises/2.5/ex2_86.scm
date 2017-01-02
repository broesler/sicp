;;==============================================================================
;;     File: ex2_86.scm
;;  Created: 01/01/2017, 14:14
;;   Author: Bernie Roesler
;;
;;  Description: Complex numbers with generic parts
;;
;;==============================================================================
(load "ex2_85.scm")

;;; Test Code:
(printval (square-root (make-scheme-number -1))) 
; Value: (complex rectangular 0 . 1)

(printval (add (make-complex-from-real-imag (make-scheme-number 4) 
                                            (make-rational 2 4))
               (make-scheme-number 4)))
;; (4 + 2/4i) + 4 = (4 + 1/2i) + 4 = (4 + 1/2i) + (4 + 0i) = 8 + 1/2i
;; ==> ; Value: (complex rectangular 8 . .5)

(printval (sub (make-complex-from-real-imag (make-rational 3 2) 
                                            (make-scheme-number 3))
               (make-complex-from-real-imag (make-rational 1 2) 
                                            (make-real 3))))
;; (1.5 + 3i) - (1/2 + 3i) = (1.5 - 1/2) + (3 - 3)i = (1.0 + 0i)
;; ==> ; Value: 1

(define bb (make-complex-from-real-imag (make-rational 3 4) 
                                        (make-rational 2 5)))
(printval (add bb bb)) 
; Value: (complex rectangular (rational 3 . 2) rational 4 . 5)
;;==============================================================================
;;==============================================================================
