;;==============================================================================
;;     File: sequence_operations.scm
;;  Created: 11/12/2016, 13:36
;;   Author: Bernie Roesler
;;
;;  Description: Conventional interface book code examples 
;;
;;==============================================================================

;;; Signal-flow operations:
;;; [enumerate] --> [filter] --> [map] --> [accumulate]
;;; [enumerate] --> [map] --> [filter] --> [accumulate]

;;; Define some steps of a sequence operation
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(printval (filter odd? (list 1 2 3 4 5))) ; Value: (1 3 5)

;;; Accumulate step
(define (accumulate op initial sequence)
  (if (null? sequence) 
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(printval (accumulate + 0 (list 1 2 3 4 5))) ; Value: 15
(printval (accumulate * 1 (list 1 2 3 4 5))) ; Value: 120
(printval (accumulate cons nil (list 1 2 3 4 5))) ; Value: (1 2 3 4 5)

;;; Enumerate items in a sequence
(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(printval (enumerate-interval 2 7)) ; Value: (2 3 4 5 6 7)

;;; Enumerate leaves in a tree
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(printval (enumerate-tree (list 1 (list 2 (list 3 4)) 5))) ; Value: (1 2 3 4 5)
;;==============================================================================
;;==============================================================================
