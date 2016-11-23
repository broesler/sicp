;;==============================================================================
;;     File: ps4_ex2.scm
;;  Created: 11/23/2016, 10:47
;;   Author: Bernie Roesler
;;
;;  Description: Writing a cleaner output from smeval
;;
;;==============================================================================
(load "ps4go.scm")

;------------------------------------------------------------------------------- 
;        2.A simple? predicate
;-------------------------------------------------------------------------------
(define (save-this-step? step-number body)
  (simple? (expression-of-body body)))

;;; Return true if the given expression contains only numbers, truth values,
;;; variables, and combinations
;;;   Operator: can be variable? or combination?
;;;   Operands: can be number?, boolean?, variable?, or combination?
(define (simple? expr) 
  (or (number? expr) 
      (boolean? expr)
      (variable? expr)
      (and (combination? expr)
           (simple? (operator expr)) 
           (every? (operands expr) simple?))))

(define (every? elems pred)
  (or (null? elems)
      (and (pred (car elems))
           (every? (cdr elems) pred))))

;;; Uncomment ot print all steps
; (define (simple? expr) #t)

;;; Test code:
(smeval
  '((define fact
      (lambda (n)
        (if (= n 0)
          1
          (* n (fact (- n 1))))))
    (fact 4)))

;;; Output:
    ; ;==(0)==>((define fact
    ;    (lambda (n)
    ;      (if (= n 0)
    ;          1
    ;          (* n (fact (- n 1))))))
    ;  (fact 4))
    ;
    ;
    ; ;==(5)==>((define fact
    ;    (lambda (n)
    ;      (if (= n 0)
    ;          1
    ;          (* n (fact (- n 1))))))
    ;  (define n#0
    ;    4)
    ;  (* n#0 (fact (- n#0 1))))
    ;
    ;
    ; ;==(6)==>((define fact
    ;    (lambda (n)
    ;      (if (= n 0)
    ;          1
    ;          (* n (fact (- n 1))))))
    ;  (define n#0
    ;    4)
    ;  (* 4 (fact (- n#0 1))))
    ;
    ;
    ; ;==(7)==>((define fact
    ;    (lambda (n)
    ;      (if (= n 0)
    ;          1
    ;          (* n (fact (- n 1))))))
    ;  (define n#0
    ;    4)
    ;  (* 4 (fact (- 4 1))))
    ;
    ;
    ; ;==(8)==>((define fact
    ;    (lambda (n)
    ;      (if (= n 0)
    ;          1
    ;          (* n (fact (- n 1))))))
    ;  (define n#0
    ;    4)
    ;  (* 4 (fact 3)))
    ;
    ;
    ; ;==(13)==>((define fact
    ;    (lambda (n)
    ;      (if (= n 0)
    ;          1
    ;          (* n (fact (- n 1))))))
    ;  (define n#0
    ;    4)
    ;  (define n#1
    ;    3)
    ;  (* 4 (* n#1 (fact (- n#1 1)))))
    ;
    ;
    ; ;==(14)==>((define fact
    ;    (lambda (n)
    ;      (if (= n 0)
    ;          1
    ;          (* n (fact (- n 1))))))
    ;  (define n#0
    ;    4)
    ;  (define n#1
    ;    3)
    ;  (* 4 (* 3 (fact (- n#1 1)))))
    ;
    ;
    ; ;==(15)==>((define fact
    ;    (lambda (n)
    ;      (if (= n 0)
    ;          1
    ;          (* n (fact (- n 1))))))
    ;  (define n#0
    ;    4)
    ;  (define n#1
    ;    3)
    ;  (* 4 (* 3 (fact (- 3 1)))))
    ;
    ;
    ; ;==(16)==>((define fact
    ;    (lambda (n)
    ;      (if (= n 0)
    ;          1
    ;          (* n (fact (- n 1))))))
    ;  (define n#0
    ;    4)
    ;  (define n#1
    ;    3)
    ;  (* 4 (* 3 (fact 2))))
    ;
    ;
    ; ;==(21)==>((define fact
    ;    (lambda (n)
    ;      (if (= n 0)
    ;          1
    ;          (* n (fact (- n 1))))))
    ;  (define n#0
    ;    4)
    ;  (define n#1
    ;    3)
    ;  (define n#2
    ;    2)
    ;  (* 4 (* 3 (* n#2 (fact (- n#2 1))))))
    ;
    ;
    ; ;==(22)==>((define fact
    ;    (lambda (n)
    ;      (if (= n 0)
    ;          1
    ;          (* n (fact (- n 1))))))
    ;  (define n#0
    ;    4)
    ;  (define n#1
    ;    3)
    ;  (define n#2
    ;    2)
    ;  (* 4 (* 3 (* 2 (fact (- n#2 1))))))
    ;
    ;
    ; ;==(23)==>((define fact
    ;    (lambda (n)
    ;      (if (= n 0)
    ;          1
    ;          (* n (fact (- n 1))))))
    ;  (define n#0
    ;    4)
    ;  (define n#1
    ;    3)
    ;  (define n#2
    ;    2)
    ;  (* 4 (* 3 (* 2 (fact (- 2 1))))))
    ;
    ;
    ; ;==(24)==>((define fact
    ;    (lambda (n)
    ;      (if (= n 0)
    ;          1
    ;          (* n (fact (- n 1))))))
    ;  (define n#0
    ;    4)
    ;  (define n#1
    ;    3)
    ;  (define n#2
    ;    2)
    ;  (* 4 (* 3 (* 2 (fact 1)))))
    ;
    ;
    ; ;==(29)==>((define fact
    ;    (lambda (n)
    ;      (if (= n 0)
    ;          1
    ;          (* n (fact (- n 1))))))
    ;  (define n#0
    ;    4)
    ;  (define n#1
    ;    3)
    ;  (define n#2
    ;    2)
    ;  (define n#3
    ;    1)
    ;  (* 4 (* 3 (* 2 (* n#3 (fact (- n#3 1)))))))
    ;
    ;
    ; ;==(30)==>((define fact
    ;    (lambda (n)
    ;      (if (= n 0)
    ;          1
    ;          (* n (fact (- n 1))))))
    ;  (define n#0
    ;    4)
    ;  (define n#1
    ;    3)
    ;  (define n#2
    ;    2)
    ;  (define n#3
    ;    1)
    ;  (* 4 (* 3 (* 2 (* 1 (fact (- n#3 1)))))))
    ;
    ;
    ; ;==(31)==>((define fact
    ;    (lambda (n)
    ;      (if (= n 0)
    ;          1
    ;          (* n (fact (- n 1))))))
    ;  (define n#0
    ;    4)
    ;  (define n#1
    ;    3)
    ;  (define n#2
    ;    2)
    ;  (define n#3
    ;    1)
    ;  (* 4 (* 3 (* 2 (* 1 (fact (- 1 1)))))))
    ;
    ;
    ; ;==(32)==>((define fact
    ;    (lambda (n)
    ;      (if (= n 0)
    ;          1
    ;          (* n (fact (- n 1))))))
    ;  (define n#0
    ;    4)
    ;  (define n#1
    ;    3)
    ;  (define n#2
    ;    2)
    ;  (define n#3
    ;    1)
    ;  (* 4 (* 3 (* 2 (* 1 (fact 0))))))
    ;
    ;
    ; ;==(37)==>((define fact
    ;    (lambda (n)
    ;      (if (= n 0)
    ;          1
    ;          (* n (fact (- n 1))))))
    ;  (define n#0
    ;    4)
    ;  (define n#1
    ;    3)
    ;  (define n#2
    ;    2)
    ;  (define n#3
    ;    1)
    ;  (define n#4
    ;    0)
    ;  (* 4 (* 3 (* 2 (* 1 1)))))
    ;
    ;
    ; ;==(38)==>((define fact
    ;    (lambda (n)
    ;      (if (= n 0)
    ;          1
    ;          (* n (fact (- n 1))))))
    ;  (define n#0
    ;    4)
    ;  (define n#1
    ;    3)
    ;  (define n#2
    ;    2)
    ;  (define n#3
    ;    1)
    ;  (define n#4
    ;    0)
    ;  (* 4 (* 3 (* 2 1))))
    ;
    ;
    ; ;==(39)==>((define fact
    ;    (lambda (n)
    ;      (if (= n 0)
    ;          1
    ;          (* n (fact (- n 1))))))
    ;  (define n#0
    ;    4)
    ;  (define n#1
    ;    3)
    ;  (define n#2
    ;    2)
    ;  (define n#3
    ;    1)
    ;  (define n#4
    ;    0)
    ;  (* 4 (* 3 2)))
    ;
    ;
    ; ;==(40)==>(* 4 6)
    ;
    ;
    ; ;==(41)==>24
    ;
    ;
    ; ;==(42)==>24
    ;
    ; Syntactic Value was returned
    ; ;... done
    ; ;Value: 24

;------------------------------------------------------------------------------- 
;        2.B just get expressions
;-------------------------------------------------------------------------------
;;; Definition from smeval.scm:
; (define (print-stepped-message step-number body)
;   (begin (newline)
;          (newline)
;          (display ";==(")
;          (display step-number)
;          (display ")==>")
;          (pp body)))

;;; Modified to only print the expression (and get rid of extraneous newlines)
(define (print-stepped-message step-number body)
  (begin (display ";==(")
         (display step-number)
         (display ")==>")
         (pp (expression-of-body body))))

;;; Recursive factorial:
(newline)
(display ";;;;;;;;;; Recursive factorial ;;;;;;;;;;")
(newline)
(smeval
  '((define fact
      (lambda (n)
        (if (= n 0)
          1
          (* n (fact (- n 1))))))
    (fact 4)))

;;; Output:
    ;==(0)==>(fact 4)
    ;==(5)==>(* n#5 (fact (- n#5 1)))
    ;==(6)==>(* 4 (fact (- n#5 1)))
    ;==(7)==>(* 4 (fact (- 4 1)))
    ;==(8)==>(* 4 (fact 3))
    ;==(13)==>(* 4 (* n#6 (fact (- n#6 1))))
    ;==(14)==>(* 4 (* 3 (fact (- n#6 1))))
    ;==(15)==>(* 4 (* 3 (fact (- 3 1))))
    ;==(16)==>(* 4 (* 3 (fact 2)))
    ;==(21)==>(* 4 (* 3 (* n#7 (fact (- n#7 1)))))
    ;==(22)==>(* 4 (* 3 (* 2 (fact (- n#7 1)))))
    ;==(23)==>(* 4 (* 3 (* 2 (fact (- 2 1)))))
    ;==(24)==>(* 4 (* 3 (* 2 (fact 1))))
    ;==(29)==>(* 4 (* 3 (* 2 (* n#8 (fact (- n#8 1))))))
    ;==(30)==>(* 4 (* 3 (* 2 (* 1 (fact (- n#8 1))))))
    ;==(31)==>(* 4 (* 3 (* 2 (* 1 (fact (- 1 1))))))
    ;==(32)==>(* 4 (* 3 (* 2 (* 1 (fact 0)))))
    ;==(37)==>(* 4 (* 3 (* 2 (* 1 1))))
    ;==(38)==>(* 4 (* 3 (* 2 1)))
    ;==(39)==>(* 4 (* 3 2))
    ;==(40)==>(* 4 6)
    ;==(41)==>24
    ;==(42)==>24

;;; Iterative factorial:
(newline)
(display ";;;;;;;;;; Iterative factorial ;;;;;;;;;;")
(newline)
(smeval
  '((define (iter-fact n)
      (define (iter n result)
        (if (<= n 0)
          result
          (iter (- n 1) (* n result))))
      (iter n 1))
    (iter-fact 4)))

;;; Output:
    ;;;;;;;;;; Iterative factorial ;;;;;;;;;;
    ;==(0)==>(iter-fact 4)
    ;==(2)==>(iter#1 n#10 1)
    ;==(3)==>(iter#1 4 1)
    ;==(8)==>(iter#1 (- n#11 1) (* n#11 result#0))
    ;==(9)==>(iter#1 (- 4 1) (* n#11 result#0))
    ;==(10)==>(iter#1 3 (* n#11 result#0))
    ;==(11)==>(iter#1 3 (* 4 result#0))
    ;==(12)==>(iter#1 3 (* 4 1))
    ;==(13)==>(iter#1 3 4)
    ;==(18)==>(iter#1 (- n#12 1) (* n#12 result#1))
    ;==(19)==>(iter#1 (- 3 1) (* n#12 result#1))
    ;==(20)==>(iter#1 2 (* n#12 result#1))
    ;==(21)==>(iter#1 2 (* 3 result#1))
    ;==(22)==>(iter#1 2 (* 3 4))
    ;==(23)==>(iter#1 2 12)
    ;==(28)==>(iter#1 (- n#13 1) (* n#13 result#2))
    ;==(29)==>(iter#1 (- 2 1) (* n#13 result#2))
    ;==(30)==>(iter#1 1 (* n#13 result#2))
    ;==(31)==>(iter#1 1 (* 2 result#2))
    ;==(32)==>(iter#1 1 (* 2 12))
    ;==(33)==>(iter#1 1 24)
    ;==(38)==>(iter#1 (- n#14 1) (* n#14 result#3))
    ;==(39)==>(iter#1 (- 1 1) (* n#14 result#3))
    ;==(40)==>(iter#1 0 (* n#14 result#3))
    ;==(41)==>(iter#1 0 (* 1 result#3))
    ;==(42)==>(iter#1 0 (* 1 24))
    ;==(43)==>(iter#1 0 24)
    ;==(48)==>result#4
    ;==(49)==>24
    ;==(50)==>24

;;; Recursive fibonacci
(newline)
(display ";;;;;;;;;; Recursive fibonacci ;;;;;;;;;;")
(newline)
(smeval
  '((define (rec-fib n)
      (if (< n 2)
        n
        (+ (rec-fib (- n 1))
           (rec-fib (- n 2)))))
    (rec-fib 4)))

;;; Output:
    ;;;;;;;;;; Recursive fibonacci ;;;;;;;;;;
    ;==(0)==>(rec-fib 4)
    ;==(5)==>(+ (rec-fib (- n#16 1)) (rec-fib (- n#16 2)))
    ;==(6)==>(+ (rec-fib (- 4 1)) (rec-fib (- n#16 2)))
    ;==(7)==>(+ (rec-fib 3) (rec-fib (- n#16 2)))
    ;==(12)==>(+ (+ (rec-fib (- n#17 1)) (rec-fib (- n#17 2))) (rec-fib (- n#16 2)))
    ;==(13)==>(+ (+ (rec-fib (- 3 1)) (rec-fib (- n#17 2))) (rec-fib (- n#16 2)))
    ;==(14)==>(+ (+ (rec-fib 2) (rec-fib (- n#17 2))) (rec-fib (- n#16 2)))
    ;==(19)==>(+ (+ (+ (rec-fib (- n#18 1)) (rec-fib (- n#18 2))) (rec-fib (- n#17 2))) (rec-fib (- n#16 2)))
    ;==(20)==>(+ (+ (+ (rec-fib (- 2 1)) (rec-fib (- n#18 2))) (rec-fib (- n#17 2))) (rec-fib (- n#16 2)))
    ;==(21)==>(+ (+ (+ (rec-fib 1) (rec-fib (- n#18 2))) (rec-fib (- n#17 2))) (rec-fib (- n#16 2)))
    ;==(26)==>(+ (+ (+ n#19 (rec-fib (- n#18 2))) (rec-fib (- n#17 2))) (rec-fib (- n#16 2)))
    ;==(27)==>(+ (+ (+ 1 (rec-fib (- n#18 2))) (rec-fib (- n#17 2))) (rec-fib (- n#16 2)))
    ;==(28)==>(+ (+ (+ 1 (rec-fib (- 2 2))) (rec-fib (- n#17 2))) (rec-fib (- n#16 2)))
    ;==(29)==>(+ (+ (+ 1 (rec-fib 0)) (rec-fib (- n#17 2))) (rec-fib (- n#16 2)))
    ;==(34)==>(+ (+ (+ 1 n#20) (rec-fib (- n#17 2))) (rec-fib (- n#16 2)))
    ;==(35)==>(+ (+ (+ 1 0) (rec-fib (- n#17 2))) (rec-fib (- n#16 2)))
    ;==(36)==>(+ (+ 1 (rec-fib (- n#17 2))) (rec-fib (- n#16 2)))
    ;==(37)==>(+ (+ 1 (rec-fib (- 3 2))) (rec-fib (- n#16 2)))
    ;==(38)==>(+ (+ 1 (rec-fib 1)) (rec-fib (- n#16 2)))
    ;==(43)==>(+ (+ 1 n#21) (rec-fib (- n#16 2)))
    ;==(44)==>(+ (+ 1 1) (rec-fib (- n#16 2)))
    ;==(45)==>(+ 2 (rec-fib (- n#16 2)))
    ;==(46)==>(+ 2 (rec-fib (- 4 2)))
    ;==(47)==>(+ 2 (rec-fib 2))
    ;==(52)==>(+ 2 (+ (rec-fib (- n#22 1)) (rec-fib (- n#22 2))))
    ;==(53)==>(+ 2 (+ (rec-fib (- 2 1)) (rec-fib (- n#22 2))))
    ;==(54)==>(+ 2 (+ (rec-fib 1) (rec-fib (- n#22 2))))
    ;==(59)==>(+ 2 (+ n#23 (rec-fib (- n#22 2))))
    ;==(60)==>(+ 2 (+ 1 (rec-fib (- n#22 2))))
    ;==(61)==>(+ 2 (+ 1 (rec-fib (- 2 2))))
    ;==(62)==>(+ 2 (+ 1 (rec-fib 0)))
    ;==(67)==>(+ 2 (+ 1 n#24))
    ;==(68)==>(+ 2 (+ 1 0))
    ;==(69)==>(+ 2 1)
    ;==(70)==>3


;;; Iterative fibonacci
(newline)
(display ";;;;;;;;;; Iterative fibonacci ;;;;;;;;;;")
(newline)
(smeval
  '((define (iter-fib n)
      (define (iter count a b)
        (if (= count 0)
          b
          (iter (- count 1) (+ a b) a)))
      (iter n 1 0))
    (iter-fib 4)))

;;; Output:
    ;;;;;;;;;; Iterative fibonacci ;;;;;;;;;;
    ;==(0)==>(iter-fib 4)
    ;==(2)==>(iter#3 n#25 1 0)
    ;==(3)==>(iter#3 4 1 0)
    ;==(8)==>(iter#3 (- count#0 1) (+ a#0 b#0) a#0)
    ;==(9)==>(iter#3 (- 4 1) (+ a#0 b#0) a#0)
    ;==(10)==>(iter#3 3 (+ a#0 b#0) a#0)
    ;==(11)==>(iter#3 3 (+ 1 b#0) a#0)
    ;==(12)==>(iter#3 3 (+ 1 0) a#0)
    ;==(13)==>(iter#3 3 1 a#0)
    ;==(14)==>(iter#3 3 1 1)
    ;==(19)==>(iter#3 (- count#1 1) (+ a#1 b#1) a#1)
    ;==(20)==>(iter#3 (- 3 1) (+ a#1 b#1) a#1)
    ;==(21)==>(iter#3 2 (+ a#1 b#1) a#1)
    ;==(22)==>(iter#3 2 (+ 1 b#1) a#1)
    ;==(23)==>(iter#3 2 (+ 1 1) a#1)
    ;==(24)==>(iter#3 2 2 a#1)
    ;==(25)==>(iter#3 2 2 1)
    ;==(30)==>(iter#3 (- count#2 1) (+ a#2 b#2) a#2)
    ;==(31)==>(iter#3 (- 2 1) (+ a#2 b#2) a#2)
    ;==(32)==>(iter#3 1 (+ a#2 b#2) a#2)
    ;==(33)==>(iter#3 1 (+ 2 b#2) a#2)
    ;==(34)==>(iter#3 1 (+ 2 1) a#2)
    ;==(35)==>(iter#3 1 3 a#2)
    ;==(36)==>(iter#3 1 3 2)
    ;==(41)==>(iter#3 (- count#3 1) (+ a#3 b#3) a#3)
    ;==(42)==>(iter#3 (- 1 1) (+ a#3 b#3) a#3)
    ;==(43)==>(iter#3 0 (+ a#3 b#3) a#3)
    ;==(44)==>(iter#3 0 (+ 3 b#3) a#3)
    ;==(45)==>(iter#3 0 (+ 3 2) a#3)
    ;==(46)==>(iter#3 0 5 a#3)
    ;==(47)==>(iter#3 0 5 3)
    ;==(52)==>b#4
    ;==(53)==>3

;------------------------------------------------------------------------------- 
;        2.C get rid of variables as operands
;-------------------------------------------------------------------------------
;;; Return true if the given expression contains only numbers, truth values,
;;; variables, and combinations
;;;   Operator: can be variable? or combination?
;;;   Operands: can be number?, boolean?, (not variable?), or combination?
(define (simple? expr) 
  (or (number? expr) 
      (boolean? expr)
      (variable? expr)
      (and (combination? expr)
           (simple? (operator expr)) 
           (every? (operands expr) 
                   (lambda (x)
                     (and (not (variable? x))
                          (simple? x)))))))

;;; Recursive factorial:
(newline)
(display ";;;;;;;;;; Recursive factorial ;;;;;;;;;;")
(newline)
(smeval
  '((define fact
      (lambda (n)
        (if (= n 0)
          1
          (* n (fact (- n 1))))))
    (fact 4)))

;;; Output:
    ;;;;;;;;;; Recursive factorial ;;;;;;;;;;
    ;==(0)==>(fact 4)
    ;==(7)==>(* 4 (fact (- 4 1)))
    ;==(8)==>(* 4 (fact 3))
    ;==(15)==>(* 4 (* 3 (fact (- 3 1))))
    ;==(16)==>(* 4 (* 3 (fact 2)))
    ;==(23)==>(* 4 (* 3 (* 2 (fact (- 2 1)))))
    ;==(24)==>(* 4 (* 3 (* 2 (fact 1))))
    ;==(31)==>(* 4 (* 3 (* 2 (* 1 (fact (- 1 1))))))
    ;==(32)==>(* 4 (* 3 (* 2 (* 1 (fact 0)))))
    ;==(37)==>(* 4 (* 3 (* 2 (* 1 1))))
    ;==(38)==>(* 4 (* 3 (* 2 1)))
    ;==(39)==>(* 4 (* 3 2))
    ;==(40)==>(* 4 6)
    ;==(41)==>24
    ;==(42)==>24


;;; Iterative factorial:
(newline)
(display ";;;;;;;;;; Iterative factorial ;;;;;;;;;;")
(newline)
(smeval
  '((define (iter-fact n)
      (define (iter n result)
        (if (<= n 0)
          result
          (iter (- n 1) (* n result))))
      (iter n 1))
    (iter-fact 4)))

;;; Output:
    ;;;;;;;;;; Iterative factorial ;;;;;;;;;;
    ;==(0)==>(iter-fact 4)
    ;==(3)==>(iter#5 4 1)
    ;==(12)==>(iter#5 3 (* 4 1))
    ;==(13)==>(iter#5 3 4)
    ;==(22)==>(iter#5 2 (* 3 4))
    ;==(23)==>(iter#5 2 12)
    ;==(32)==>(iter#5 1 (* 2 12))
    ;==(33)==>(iter#5 1 24)
    ;==(42)==>(iter#5 0 (* 1 24))
    ;==(43)==>(iter#5 0 24)
    ;==(48)==>result#9
    ;==(49)==>24


;;; Recursive fibonacci
(newline)
(display ";;;;;;;;;; Recursive fibonacci ;;;;;;;;;;")
(newline)
(smeval
  '((define (rec-fib n)
      (if (< n 2)
        n
        (+ (rec-fib (- n 1))
           (rec-fib (- n 2)))))
    (rec-fib 4)))

;;; Output:
    ;;;;;;;;;; Recursive fibonacci ;;;;;;;;;;
    ;==(0)==>(rec-fib 4)
    ;==(46)==>(+ 2 (rec-fib (- 4 2)))
    ;==(47)==>(+ 2 (rec-fib 2))
    ;==(61)==>(+ 2 (+ 1 (rec-fib (- 2 2))))
    ;==(62)==>(+ 2 (+ 1 (rec-fib 0)))
    ;==(68)==>(+ 2 (+ 1 0))
    ;==(69)==>(+ 2 1)
    ;==(70)==>3
    ;==(71)==>3

;;; A lot of intermediate variables get used in this call 
;;; (i.e. (rec-fib (- n 1)), and these variables are eliminated by our
;;; re-definition of (simple?) to exclude variables from the operand positions.

;;; Iterative fibonacci
(newline)
(display ";;;;;;;;;; Iterative fibonacci ;;;;;;;;;;")
(newline)
(smeval
  '((define (iter-fib n)
      (define (iter count a b)
        (if (= count 0)
          b
          (iter (- count 1) (+ a b) a)))
      (iter n 1 0))
    (iter-fib 4)))

;;; Output:
    ;;;;;;;;;; Iterative fibonacci ;;;;;;;;;;
    ;==(0)==>(iter-fib 4)
    ;==(3)==>(iter#7 4 1 0)
    ;==(14)==>(iter#7 3 1 1)
    ;==(25)==>(iter#7 2 2 1)
    ;==(36)==>(iter#7 1 3 2)
    ;==(47)==>(iter#7 0 5 3)
    ;==(52)==>b#9
    ;==(53)==>3
    ;==(54)==>3

;;==============================================================================
;;==============================================================================
