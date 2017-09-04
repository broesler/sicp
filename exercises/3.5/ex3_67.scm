;;==============================================================================
;;     File: ex3_67.scm
;;  Created: 09/04/2017, 16:51
;;   Author: Bernie Roesler
;;
;;  Description: Stream of pairs (i,j) without constraint i ≤ j
;;
;;==============================================================================
(load "streams.scm")

;; Original pairs (where i ≤ j)
(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

;; New pairs with all (i,j)
(define (all-pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (interleave
        (stream-map (lambda (x) (list (stream-car s) x))
                    (stream-cdr t))
        (all-pairs (stream-cdr s) (stream-cdr t)))
      ; Include car of t with rest of s
      (stream-map (lambda (x) (list x (stream-car t)))
                    (stream-cdr s)))))

(display "pairs:")
(display-stream-n (pairs integers integers) 10)
(display "all-pairs:")
(display-stream-n (all-pairs integers integers) 10)


;;==============================================================================
;;==============================================================================
