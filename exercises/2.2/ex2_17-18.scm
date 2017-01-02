;;==============================================================================
;;     File: ex2_17-18.scm
;;  Created: 11/09/2016, 22:12
;;   Author: Bernie Roesler
;;
;;  Description: List operations
;;
;;==============================================================================

;------------------------------------------------------------------------------- 
;        Ex 2.17 last-pair
;-------------------------------------------------------------------------------
;;; last-pair: return only the last element of a given list
;; linear recursive
(define (last-pair list1)
  (cond ((null? list1) '())   ; check for empty list
        ((null? (cdr list1))  ; we're on the last element
         list1)               ; return value as a list
        (else (last-pair (cdr list1)))))

;; Test code:
(define sq (list 1 4 9 16 25))
(printval (last-pair '()))                 ; Value: ()
(printval (last-pair (list 1)))            ; Value: (1)
(printval (last-pair (list 23 72 149 34))) ; Value: (34)
(printval (last-pair sq))                  ; Value: (25)

;------------------------------------------------------------------------------- 
;        Ex 2.18 reverse
;-------------------------------------------------------------------------------
(define (my-reverse list1)
  (define (rev-iter a result)
    (if (null? a)
      result
      (rev-iter (cdr a) 
                (cons (car a) result))))
  (rev-iter list1 '()))

;; Test code:
(printval (my-reverse sq)) ; Value: (25 16 9 4 1)

; Example procedure:
; (define x (list 1 2 3 4))
; (cons (car (cdr (cdr (cdr x)))) 
;       (cons (car (cdr (cdr x))) 
;             (cons (car (cdr x)) 
;                   (cons (car x) '()))))

;;==============================================================================
;;==============================================================================
