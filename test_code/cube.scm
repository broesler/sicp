(define (cube x)
(if (not (number? x))
  (error "Argument should be a number instead of " x)
  (* x x x)))
