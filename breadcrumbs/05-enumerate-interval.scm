;; constructs a list of all integers [low, high)

(define (enumerate-interval low high)
  (if (> low high)
      (list )
      (cons low (enumerate-interval (+ low 1) high))))
