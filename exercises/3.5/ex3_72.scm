;;==============================================================================
;;     File: ex3_72.scm
;;  Created: 09/04/2017, 18:42
;;   Author: Bernie Roesler
;;
;;  Description: Numbers that can be written as sum of two squares in three
;;    different ways
;;
;;==============================================================================
(load "ex3_70.scm") ; weighted-pairs

;; Generate stream of Ramanujan numbers
(define (sum-of-squares)
  (define (square x) (* x x))
  (define (weight p)
    (+ (square (car p)) 
       (square (cadr p))))
  (define s (weighted-pairs integers integers weight))  ; ordered pairs
  (define (search t)
    (let* ((a (stream-car t))
           (b (stream-cadr t))
           (c (stream-caddr t))
           (wa (weight a))
           (wb (weight b))
           (wc (weight c)))
      (if (and (= wa wb) (= wb wc))
        (cons-stream (list wa a b c) 
                     (search (stream-cdr (stream-cdr (stream-cdr t)))))
        (search (stream-cdr t)))))
  (search s))

(display-stream-n (sum-of-squares) 6)

;;==============================================================================
;;==============================================================================
