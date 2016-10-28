;; creates a list with the elements [1, 2, 3, 4]
;; NOTE: '() is an empty list 
(cons 1 (cons 2 (cons 3 (cons 4 '()))))

;; since that is an awful lot to write there is a short hand way 
(list 1 2 3 4)

;; also, there is way to append two list together
(append '(1) '(2 3 4))
