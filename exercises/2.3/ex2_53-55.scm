;;==============================================================================
;;     File: ex2_53-55.scm
;;  Created: 11/14/2016, 14:50
;;   Author: Bernie Roesler
;;
;;  Description: Quoting basics
;;
;;==============================================================================

;;; takes symbol and list, and returns the sublist starting with the symbol
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(printval (memq 'apple '(pear banana prune))) ; Value: #f
(printval (memq 'apple '(x (apple sauce) y apple pear))) ; Value: (apple pear)

;------------------------------------------------------------------------------- 
;        Ex 2.53
;-------------------------------------------------------------------------------
;;; What are these values?
(printval (list 'a 'b 'c)) ; Value: (a b c)

(printval (list (list 'george))) ; Value: ((george))
(printval (cdr '((x1 x2) (y1 y2)))) ; Value: ((y1 y2))

(printval (cadr '((x1 x2) (y1 y2)))) ; Value: (y1 y2)
(printval (pair? (car '(a short list)))) ; Value: #f
(printval (memq 'red '((red shoes) (blue socks)))) ; Value: #f

(printval (memq 'red '(red shoes blue socks))) ; Value: (red shoes blue socks)

;------------------------------------------------------------------------------- 
;        Ex 2.54
;-------------------------------------------------------------------------------
(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b)))
         (eq? a b))
        ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        (else false)))

(printval (equal? '(this is a list) '(this is a list)))   ; Value: #t
(printval (equal? '(this is a list) '(this (is a) list))) ; Value: #f

;;==============================================================================
;;==============================================================================
