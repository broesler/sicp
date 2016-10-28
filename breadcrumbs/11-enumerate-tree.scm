;; flattens a tree to a list basically, the name is from SICP
(define (enumerate-tree lst)
  (foldr (lambda (x y)
	   (if (pair? x)
	       (append (enumerate-tree x) y) ;;key is to append not cons here
	       (cons x y)))
	 '()
	 lst))
