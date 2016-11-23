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
(define (simple? expr) #t)

(smeval
  '((define fact
      (lambda (n)
        (if (= n 0)
          1
          (* n (fact (- n 1))))))
    (fact 4)))

;;; Output
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
;;==============================================================================
;;==============================================================================
