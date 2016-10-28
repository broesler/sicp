;; returns the part of the list 'lst' for which the function 'pred?' holds true.

(define (filter pred? lst)
  (cond ((null? lst) (list ))
	((pred? (car lst)) (cons (car lst) (filter pred? (cdr lst))))
	(else (filter pred? (cdr lst)))))
