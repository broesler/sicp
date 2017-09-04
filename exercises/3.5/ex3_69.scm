;;==============================================================================
;;     File: ex3_69.scm
;;  Created: 09/04/2017, 17:14
;;   Author: Bernie Roesler
;;
;;  Description: Stream of Pythagorean triples (i,j,k) s.t. i ≤ j ≤ k, and 
;;    i^2 + j^2 = k^2
;;
;;==============================================================================
(load "streams.scm")

;; Triples (i,j,k) where i ≤ j ≤ k
(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (x) (cons (stream-car s) x))
                  (stream-cdr (pairs t u)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(display "triples:")
(display-stream-n (triples integers integers integers) 10)

(define (square x) (* x x))
(define (dist x y) (+ (square x) (square y)))
(define (pythag-triples)
  (stream-filter (lambda (triple) (= (dist (car triple) (cadr triple))
                                     (square (caddr triple))))
                 (triples integers integers integers)))

(display "Pythagorean triples:")
(display-stream-n (pythag-triples) 5)

;;==============================================================================
;;==============================================================================
