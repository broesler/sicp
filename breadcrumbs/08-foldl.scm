;;accmulates the results of applying the function f over the list

(define (foldl f cur lst)
    (if (null? lst)
	cur
	(foldl f (f cur (car lst)) (cdr lst))))
;; example: (foldl list nil '(1 2 3 4 5 6 7))
;; example also: (foldl append '() '("foo" "bar" "count" "chocula"))

