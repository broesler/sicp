;; define gives us the power to grow our language, one can add new functions that can be called elsewhere
;; for example if you needed to square a number (a) often in your program instead of writing (* a a) you could define a square function 
(define (^ a)
  (* a a))
;; now you can use (^ a) instead of (* a a)

;; defines a function 'add' that takes in two parameters a and b
;; returns the result of adding a and b together
(define (add a b)
  (+ a b))


;; some simple functions
(define (cube x)
 	(* x x x))

(define (quadratic a b c x)
	(+ (* a (square x)) (* b x) c))

(define (abs x)
	(cond ((< x 0) (- x))
              (else x)))

;; Sums the squared value of the larger two numbers of the input a,b, and c
(define (sum-square-large a b c)
	(cond ((and (> a b) (> c b)) (+ (square a) (square b)))
              ((and (> a b) (> c b)) (+ (square a) (square c)))
              (else (+ (square b) (square c)))))