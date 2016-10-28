;; car == the first element of the given list upon which it is called
;; cdr == the rest of the list not returned by car
(car '(1 2 3))
(cdr '(1 2 3))

;; defines a function 'iterate' that takes two parameters, f and lst.
;; f == a function
;; lst == a list
;; calls the function f on each item of the list lst and returns a new list of those results
(define (iterate f lst)
  (cond ((null? lst) '())
	(else (cons (f (car lst))
		    (iterate f (cdr lst))))))


	 


