;;==============================================================================
;;     File: ex2_21-22.scm
;;  Created: 11/10/2016, 01:18
;;   Author: Bernie Roesler
;;
;;  Description: Mapping square-list
;;
;;==============================================================================
(define x (list 1 2 3 4))

;;; Raw definition
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

;; Test code:
(printval (square-list x))

;;; Using map
(define (square-list items)
  (map (lambda (x) (square x)) items))

;; Test code:
(printval (square-list x))

;------------------------------------------------------------------------------- 
;        Ex 2.22 iterative?
;-------------------------------------------------------------------------------
;;; try to make iterative process...
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))

;; Test code:
(printval (square-list x)) ; Value: (16 9 4 1)

; NOTE Comes out in reverse! We are essentially "pushing" the squared values
; onto the stack that gets returned, but we can only traverse the list from
; start to end, so we push the first values to the end of the final printed list

;;; Try to fix it
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

;; Test code:
(printval (square-list x)) ; Value: ((((() . 1) . 4) . 9) . 16)

;; cons-ing a list with a value, instead of other way around.

;;==============================================================================
;;==============================================================================
