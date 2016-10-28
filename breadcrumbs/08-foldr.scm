;; the same as foldl, but from right to left

(define (foldr accum zero lst)
  (if (null? lst)
      zero
      (accum (car lst) (foldr accum zero (cdr lst)))))
;; example: (foldr list nil '(1 2 3 4 5 6 7))
