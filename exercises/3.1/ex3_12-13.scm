;;==============================================================================
;;     File: ex3_12-13.scm
;;  Created: 01/14/2017, 13:06
;;   Author: Bernie Roesler
;;
;;  Description: append! using mutators
;;
;;==============================================================================

;------------------------------------------------------------------------------- 
;        Ex 3.12
;-------------------------------------------------------------------------------
;;; append using constructors/selectors
(define (append x y)
  (if (null? x)
    y
    (cons (car x) (append (cdr x) y))))

;;; append! using mutators
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
(printval z) ; Value: (a b c d)
(printval (cdr x)) ; Value: (b)
(define w (append! x y))
(printval w) ; Value: (a b c d)
(printval (cdr x)) ; Value: (b c d)

;------------------------------------------------------------------------------- 
;        Ex 3.13
;-------------------------------------------------------------------------------
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))
; (last-pair z) ; Value: infinite loop!!
;;==============================================================================
;;==============================================================================
