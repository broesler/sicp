;; handy util function that takes a variable list of arguments,
;; prints them delimited by spaces, and then appends a newline at the end

(define displn 
	(lambda lst 
		(map (lambda (x) (display x) (display " ")) lst) 
		(newline)))
