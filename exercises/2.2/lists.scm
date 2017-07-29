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

;;; Map a procedure to a list (apply to each element)
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17)) ; Value: (10 2.5 11.6 17)
(map (lambda (x) (* x x)) (list 1 2 3 4)) ; Value: (1 4 9 16)

;------------------------------------------------------------------------------- 
;        Trees
;-------------------------------------------------------------------------------
(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; Test code:
(define x (cons (list 1 2) (list 3 4)))
(printval x)                ; Value: ((1 2) 3 4)
(printval (length x))       ; Value: 3
(printval (count-leaves x)) ; Value: 4

(printval (list x x))                ; Value: (((1 2) 3 4) ((1 2) 3 4))
(printval (length (list x x)))       ; Value: 2
(printval (count-leaves (list x x))) ; Value: 8
;;==============================================================================
;;==============================================================================
