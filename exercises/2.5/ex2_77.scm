;;==============================================================================
;;     File: ex2_77.scm
;;  Created: 12/14/2016, 22:55
;;   Author: Bernie Roesler
;;
;;  Description: Louis Reasoner fails
;;
;;==============================================================================
(load "generic_arithmetic.scm")

;;; Louis' object:
(define z (make-complex-from-real-imag 3 4))
z ; Value: (complex rectangular 3 . 4)

;;; Test code:
(magnitude z)

;;; Steps to actually apply (magnitude z) are as follows:
;;; (magnitude (complex rectangular 3 . 4))
;;; (apply-generic 'magnitude (complex rectangular 3 . 4))
;;;   (type-tags <= (map type-tag ((complex rectangular 3 . 4))))
;;;   (type-tags <= '(complex))
;;;   (proc <= (get 'magnitude '(complex)))
;;;   (proc <= (apply-generic 'magnitude z))
;;;     (apply (apply-generic 'magnitude z) (map contents z))
;;;     (apply (apply-generic 'magnitude z) ((rectangular 3 . 4)))
;;;     (apply-generic 'magnitude (rectangular 3 . 4))
;;;       (type-tags <= (map type-tag ((rectangular 3 . 4))))
;;;       (type-tags <= '(rectangular))
;;;       (proc <= (get 'magnitude '(retangular)))
;;;       (proc <= (sqrt (+ (square (real-part z)) 
;;;                         (square (imag-part z)))))
;;;         (apply (sqrt (+ (square (real-part z)) 
;;;                         (square (imag-part z)))) 
;;;                (map contents 
;;;                     (('rectangular 3 . 4))))
;;;         (sqrt (+ (square (real-part (3 . 4))) 
;;;                  (square (imag-part (3 . 4)))))
;;;         (sqrt (+ (square (car (3 . 4))) 
;;;                  (square (cdr (3 . 4)))))
;;;         (sqrt (+ (square 3) (square 4)))
;;;         (sqrt (+ 9 16))
;;;         (sqrt 25)
; Value: 5

;;; (apply-generic) is applied twice! 
;;; once to strip 'complex, and again to strip 'rectangular
;;==============================================================================
;;==============================================================================
