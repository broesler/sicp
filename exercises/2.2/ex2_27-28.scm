;;==============================================================================
;;     File: ex_27-28.scm
;;  Created: 11/10/2016, 17:39
;;   Author: Bernie Roesler
;;
;;  Description: Modify reverse to deep-reverse
;;
;;==============================================================================

;;; Regular shallow reverse (iterative)
; (define (reverse list1)
;   (define (rev-iter x result)
;     (if (null? x)
;       result
;       (rev-iter (cdr x) (cons (car x) result))))
;   (rev-iter list1 '()))

;;; Shallow reverse (recursive)
(define (reverse x)
  (if (null? x)
    x
    (append (reverse (cdr x)) 
            (list (car x))))) 

;;; deep-reverse
(define (deep-reverse x)
  (cond ((null? x) x)
        ((not (pair? x)) x) ; stopping condition for hitting leaves
        (else (append (deep-reverse (cdr x))
                      (list (deep-reverse (car x)))))))

;; Test code:
;; Base case: needs to work for simple list
(printval (reverse '(1 2 3 4))) ; Value: (4 3 2 1)
(printval (deep-reverse '(1 2 3 4))) ; Value: (4 3 2 1)

;; tree case:
(define x (list (list 1 2) (list 3 4)))
(printval x)                ; Value: ((1 2) (3 4))
(printval (reverse x))      ; Value: ((3 4) (1 2))
(printval (deep-reverse x)) ; Value: ((4 3) (2 1))

;------------------------------------------------------------------------------- 
;        Ex 2.28 fringe 
;-------------------------------------------------------------------------------
;;; like deep-reverse, but not reversed, and all in one list
(define (fringe x)
  (cond ((null? x) x)
        ((not (pair? x)) (list x))          ; return a list so we can append
        (else (append (fringe (car x)) 
                      (fringe (cdr x))))))

;;; Test code:
(printval (fringe x))          ; Value: (1 2 3 4)
(printval (fringe (list x x))) ; Value: (1 2 3 4 1 2 3 4)
;;==============================================================================
;;==============================================================================
