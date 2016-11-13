;;==============================================================================
;;     File: ex2_36.scm
;;  Created: 11/13/2016, 16:44
;;   Author: Bernie Roesler
;;
;;  Description: accumulate-n with third argument
;;
;;==============================================================================
(load "sequence_operations.scm")

;;; seqs == a sequence of sequences, all assumed to have the same number of
;;; elements. It applies (op) to combine all the first elements of all the sequences,
;;; all the second elements, etc., returning a sequence of results.
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (get-first seqs))
          (accumulate-n op init (get-rest seqs)))))

(define (get-first seqs) (map car seqs))
(define (get-rest seqs) (map cdr seqs))
  
;;; Test code:
(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 s) ; Value: (22 26 30)
;;==============================================================================
;;==============================================================================
