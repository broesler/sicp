; cosine spacing 
(define lo 0)
(define hi 9)
(define pi (* 4.0 (atan 1.0)))  ; canonical definition
(define b (* 2.0 0.26035))

; return list of range
(define (range lo hi step)
  (if (>= lo hi)
    '()
    (cons lo (range (+ lo step) hi step))))

; Range stops at hi-1
(define k (range lo hi 1))

; cosine spacing
(define (cosine_space k N)
  (* (/ 1.0 2.0) 
      (- 1 
         (cos (/ (* k pi) 
                 N)))))

; calculate weights 
(define weights 
  (map (lambda (x) (cosine_space x (-1+ hi))) k))

; scale by span
(define z_vals 
  (map (lambda (x) (* b x)) weights))
