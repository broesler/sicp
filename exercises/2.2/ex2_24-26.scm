;;==============================================================================
;;     File: ex2_24-26.scm
;;  Created: 11/10/2016, 16:42
;;   Author: Bernie Roesler
;;
;;  Description: Simple tree exercises
;;
;;==============================================================================

;;; Book notes:
;; Value: ((1 2) 3 4)
(printval (cons (list 1 2) (list 3 4)))
; (cons (cons 1 (cons 2 nil)) (cons 3 (cons 4 nil)))

;; Value: ((1 2) (3 4)) ; NOTE the "list" vs "cons" above
(printval (list (list 1 2) (list 3 4)))
; (cons (cons 1 (cons 2 nil)) (cons (cons 3 (cons 4 nil)) nil))

;------------------------------------------------------------------------------- 
;        Ex 2.24
;-------------------------------------------------------------------------------
(define x (list 1 (list 2 (list 3 4))))
(printval x) ; Value: (1 (2 (3 4)))
; (cons 1 (cons (cons 2 (cons (cons 3 (cons 4 nil)) nil)) nil))

;------------------------------------------------------------------------------- 
;        Ex 2.25 pick 7
;-------------------------------------------------------------------------------
(define x (list 1 3 (list 5 7) 9))
(printval (car (cdr (car (cdr (cdr x)))))) ; Value: 7

(define x (list (list 7)))
(printval (car (car x))) ; Value: 7

(define x (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(printval (cadr (cadr (cadr (cadr (cadr (cadr x)))))))

;------------------------------------------------------------------------------- 
;        Ex 2.26
;-------------------------------------------------------------------------------
(define x (list 1 2 3))
(define y (list 4 5 6))

(printval (append x y)) ; Value: (1 2 3 4 5 6)
(printval (cons x y))   ; Value: ((1 2 3) 4 5 6)
(printval (list x y))   ; Value: ((1 2 3) (4 5 6))
;;==============================================================================
;;==============================================================================
