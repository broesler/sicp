;;==============================================================================
;;     File: ex3_71.scm
;;  Created: 09/04/2017, 18:14
;;   Author: Bernie Roesler
;;
;;  Description: Ramanujan numbers (i,j) s.t. i^3 + j^3 = i^3 + j^3 for
;;    different pairs of (i,j)
;;
;;==============================================================================
(load "ex3_70.scm")

;; Generate stream of Ramanujan numbers
(define (ramanujan-s)
  (define (cube x) (* x x x))
  (define (weight p)
    (+ (cube (car p)) 
       (cube (cadr p))))
  (define s (weighted-pairs integers integers weight))  ; ordered pairs
  (define (search t)
    (let* ((a (stream-car t))
           (b (stream-cadr t))
           (wa (weight a))
           (wb (weight b)))
      (if (= wa wb)
        (cons-stream (list wa a b) 
                     (search (stream-cdr (stream-cdr t))))
        (search (stream-cdr t)))))
  (search s))

(display "stream-s:")
(display-stream-n (ramanujan-s) 6)


;;==============================================================================
;;==============================================================================
