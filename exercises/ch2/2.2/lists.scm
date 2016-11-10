;;==============================================================================
;;     File: lists.scm
;;  Created: 11/09/2016, 21:59
;;   Author: Bernie Roesler
;;
;;  Description: Simple list operations
;;
;;==============================================================================

;; Build a list
(cons 1
      (cons 2
            (cons 3
                  (cons 4 '())))) ; Value: (1 2 3 4)

; syntactic sugar
(list 1 2 3 4)

(define one-through-four (list 1 2 3 4))

(car one-through-four) ; Value: 1
(cdr one-through-four) ; Value: (2 3 4)
(car (cdr one-through-four)) ; Value: 2
(cons 10 one-through-four) ;  Value: (10 1 2 3 4)

;------------------------------------------------------------------------------- 
;        List operations
;-------------------------------------------------------------------------------
;;; return item n in the list (0-indexed by choice)
(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))
(list-ref squares 3) ; Value: 16

;;; get length of list (linear recursive)
(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))
(length odds) ; Value: 4

;;; length (iterative)
(define (length items)
  (define (length-iter a count)
    (if (null? a)
      count
      (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(length odds) ; Value: 4

;;; append lists together (recursively)
(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) 
          (append (cdr list1) list2))))

(append squares odds) ; Value: (1 4 9 16 25 1 3 5 7)

;;==============================================================================
;;==============================================================================
