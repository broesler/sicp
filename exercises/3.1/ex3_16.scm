;;==============================================================================
;;     File: ex3_16.scm
;;  Created: 01/14/2017, 13:47
;;   Author: Bernie Roesler
;;
;;  Description: Ben Bitdiddle's incorrect count-pairs
;;
;;==============================================================================
(load "ex3_12-13.scm") ; make-cycle

;;; Defined in exercise
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;;; Test code:
(define x (list 'a 'b))

(define z3 (cons x 'c))
(printval (count-pairs z3)) ; Value: 3

(define z4 (cons x (cdr x)))
(printval (count-pairs z4)) ; Value: 4

(define a (list 'a))
(define aa (cons a a))
(define z7 (cons aa aa))
(printval (count-pairs z7)) ; Value: 7

(define zinf (make-cycle (list 'a 'b 'c)))
; (count-pairs zinf) ;Aborting!: maximum recursion depth exceeded

;------------------------------------------------------------------------------- 
;        Ex 3.17: properly count pairs
;-------------------------------------------------------------------------------
;;; Traverse the structure, maintaining an auxiliary structure to track which
;;; pairs have already been counted. 
;;; Build a list of pointers to each "counted" pair, then return the length of
;;; the list.
(define (count-pairs x)
  (let ((counted '()))
    (define (count x)
      (cond ((or (not (pair? x)) 
                 (memq x counted)) 
             0)
            (else (set! counted (cons x counted))
                  (+ (count (car x))
                     (count (cdr x))
                     1))))
   (count x)))

(printval (count-pairs z3)) ; Value: 3
(printval (count-pairs z4)) ; Value: 3
(printval (count-pairs z7)) ; Value: 3
;;==============================================================================
;;==============================================================================
