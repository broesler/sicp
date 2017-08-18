;;==============================================================================
;;     File: ex3_65.scm
;;  Created: 08/18/2017, 14:19
;;   Author: Bernie Roesler
;;
;;  Description: ln(2) series
;;
;;==============================================================================
(load "ex3_55.scm") ; partial-sums

;; ln(2) = 1 - 1/2 + 1/3 - 1/4 + ...

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-1 (partial-sums (ln2-summands 1)))
(define ln2-2 (euler-transform ln2-1))
(define ln2-3 (accelerated-sequence euler-transform ln2-1))

;; Count steps until tolerance achieved
(define tol 1e-6)

(define (count-steps s tol)
  (let ((count 0)
        (max_count 100))
    (define (stream-limit s tol)
      (let ((a (stream-car s))
            (b (stream-cadr s)))
        (if (or (< (abs (- a b)) tol) 
                (> count max_count))
          'done
          (begin
            (set! count (+ count 1))
            (stream-limit (stream-cdr s) tol)))))
    (stream-limit s tol)
    count))

(printval (count-steps ln2-1 tol)) ; Value: 101
(printval (count-steps ln2-2 tol)) ; Value:  60
(printval (count-steps ln2-3 tol)) ; Value:   4 DAMN!
;;==============================================================================
;;==============================================================================
