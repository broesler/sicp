;;==============================================================================
;;     File: ex3_70.scm
;;  Created: 09/04/2017, 17:37
;;   Author: Bernie Roesler
;;
;;  Description: Merge streams of pairs according to weight function
;;
;;==============================================================================
(load "streams.scm")

;; Merge streams of pairs according to weight function
(define (weighted-merge p1 p2 weight)
  (cond ((stream-null? p1) p2)
        ((stream-null? p2) p1)
        (else
          (let* ((p1car (stream-car p1))
                 (p2car (stream-car p2))
                 (w1 (weight p1car))
                 (w2 (weight p2car)))
            (cond ((< w1 w2)
                   (cons-stream p1car 
                                (weighted-merge (stream-cdr p1) 
                                                p2 
                                                weight)))
                  ((> w1 w2)
                   (cons-stream p2car 
                                (weighted-merge p1 
                                                (stream-cdr p2) 
                                                weight)))
                  (else
                    (cons-stream p1car
                                 (weighted-merge (stream-cdr p1)
                                                 (stream-cdr p2) 
                                                 weight))))))))

;; Create stream of pairs ordered by weight
(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (weighted-merge
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight) 
    weight)))

;; (a) (i,j) s.t. i ≤ j and order is i + j
(define (weight p)
  (+ (car p) (cadr p)))
(define s (weighted-pairs integers integers weight))
(display "i+j:")
(display-stream-n s 20)

;; (b) (i,j) s.t. i ≤ j, neither is divisible by 2, 3, or 5, order by 2i+3j+5ij
(define (weight p)
  (+ (* 2 (car p)) 
     (* 3 (cadr p)) 
     (* 5 (car p) (cadr p))))

(define (divisible? x)
  (not (or (= (remainder x 2) 0)
           (= (remainder x 3) 0)
           (= (remainder x 5) 0))))

(define ndints (stream-filter divisible? integers))

(define s (weighted-pairs ndints ndints weight))
(display "2*i + 3*j + 5*i+j:")
(display-stream-n s 10)



;;==============================================================================
;;==============================================================================
