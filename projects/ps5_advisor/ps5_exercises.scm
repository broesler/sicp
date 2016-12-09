;;==============================================================================
;;     File: ps5_exercises.scm
;;  Created: 12/02/2016, 13:06
;;   Author: Bernie Roesler
;;
;;  Description: PS5 Freshman Advisor Problems
;;
;;==============================================================================
(load "match.scm")
(load "adv.scm")

;;; Pre-lab Exercise 1:
;;;   List-union checks for duplicates, append does not!
; (list-union '(1 2 3) '(3 4 5)) ; Value: (1 2 3 4 5)
; (append '(1 2 3) '(3 4 5))     ; Value: (1 2 3 3 4 5)

;;; Pre-lab Exercise 2:
;;;   Compute the product of all elements in a list using reduce
; (reduce * 1 '(1 2 3 4 5)) ; Value: 120

;;; Pre-lab Exercise 3:
;;;   Use map + reduce to compute the sum of squares of a list
; (reduce + 0 (map (lambda (x) (square x)) '(3 4 5))) ; Value: 50

;;; Pre-lab Exercise 4:
;;;   What is returned by:
; (reduce append '() '((1 2) (2 3) (4 6) (5 4)))      ; Value: (1 2 2 3 4 6 5 4)
; (reduce list-union '() '((1 2) (2 3) (4 6) (5 4)))  ; Value: (1 2 3 6 5 4)

;;; Pre-lab Exercise 5:
;;; What does this procedure do?
; (define (unique symbols)
;   (reduce list-union
;           '()
;           (map list symbols)))
;;; This procedure returns a list of unique symbols, maybe call it "unique":
; (unique '(x y z x t t y)) ; Value: (z x t y)
;;==============================================================================
;;==============================================================================
