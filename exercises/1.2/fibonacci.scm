; Fibonacci numbers (exponential, even (fib 50) is almost impossible to run!!)
; (define (fib N)
;   (if (< N 2)
;     N
;     (+ (fib (- N 1))
;        (fib (- N 2)))
;     )
;   )

; ; (linear)
; (define (fib n)
;   (fib-iter 1 0 n))
;
; (define (fib-iter a b count)
;   (if (= count 0)
;     b
;     (fib-iter (+ a b) a (- count 1))
;     )
;   )

; rewrite together
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))
