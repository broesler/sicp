;; lambda is a function without a name

(lambda (a) (+ a 1)) ;; this lambda will return a + 1, but where is a coming from? 

((lambda (a) (+ a 1)) 1)
;> 2

((lambda (a b) (+ (* a a) (* b b))) 1 2)
;> 5