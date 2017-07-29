;;==============================================================================
;;     File: ps4_ex1.scm
;;  Created: 11/22/2016, 21:31
;;   Author: Bernie Roesler
;;
;;  Description: Save all steps of computation
;;
;;==============================================================================
(load "ps4go.scm")

(define (rec-fact n)
  (if (<= n 0)
      1
      (* n (rec-fact (dec n)))))

(define (iter-fact n)
  (define (iter n result)
    (if (<= n 0)
        result
        (iter (- n 1) (* n result))))
  (iter n 1))

;------------------------------------------------------------------------------- 
;        Ex 1 -- rewrite save-this-step?
;-------------------------------------------------------------------------------
;;; Save every step:
(define (save-this-step? step-number body) #t)

;;; Eval 4! recursively
(smeval
  '((define (rec-fact n)
      (if (<= n 0)
        1
        (* n (rec-fact (dec n)))))
    (rec-fact 4)))

;;; Output:
    ;; ;==(0)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (rec-fact 4))


    ;; ;==(1)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; ((lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n)))))
    ;;   4))


    ;; ;==(2)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (if (<= n#0 0)
    ;;     1
    ;;     (* n#0 (rec-fact (dec n#0)))))


    ;; ;==(3)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (if (<= 4 0)
    ;;     1
    ;;     (* n#0 (rec-fact (dec n#0)))))


    ;; ;==(4)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (if #f
    ;;     1
    ;;     (* n#0 (rec-fact (dec n#0)))))


    ;; ;==(5)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (* n#0 (rec-fact (dec n#0))))


    ;; ;==(6)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (* 4 (rec-fact (dec n#0))))


    ;; ;==(7)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (* 4 (rec-fact (dec 4))))


    ;; ;==(8)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (* 4 (rec-fact 3)))


    ;; ;==(9)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (*
    ;;   4
    ;;   ((lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n)))))
    ;;   3)))


    ;; ;==(10)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (* 4 (if (<= n#1 0) 1 (* n#1 (rec-fact (dec n#1))))))


    ;; ;==(11)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (* 4 (if (<= 3 0) 1 (* n#1 (rec-fact (dec n#1))))))


    ;; ;==(12)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (* 4 (if #f 1 (* n#1 (rec-fact (dec n#1))))))


    ;; ;==(13)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (* 4 (* n#1 (rec-fact (dec n#1)))))


    ;; ;==(14)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (* 4 (* 3 (rec-fact (dec n#1)))))


    ;; ;==(15)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (* 4 (* 3 (rec-fact (dec 3)))))


    ;; ;==(16)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (* 4 (* 3 (rec-fact 2))))


    ;; ;==(17)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (*
    ;;   4
    ;;   (*
    ;;   3
    ;;   ((lambda (n)
    ;;       (if (<= n 0)
    ;;           1
    ;;           (* n (rec-fact (dec n)))))
    ;;     2))))


    ;; ;==(18)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (define n#2
    ;;   2)
    ;; (*
    ;;   4
    ;;   (* 3
    ;;     (if (<= n#2 0) 1 (* n#2 (rec-fact (dec n#2)))))))


    ;; ;==(19)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (define n#2
    ;;   2)
    ;; (*
    ;;   4
    ;;   (* 3 (if (<= 2 0) 1 (* n#2 (rec-fact (dec n#2)))))))


    ;; ;==(20)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (define n#2
    ;;   2)
    ;; (* 4 (* 3 (if #f 1 (* n#2 (rec-fact (dec n#2)))))))


    ;; ;==(21)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (define n#2
    ;;   2)
    ;; (* 4 (* 3 (* n#2 (rec-fact (dec n#2))))))


    ;; ;==(22)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (define n#2
    ;;   2)
    ;; (* 4 (* 3 (* 2 (rec-fact (dec n#2))))))


    ;; ;==(23)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (define n#2
    ;;   2)
    ;; (* 4 (* 3 (* 2 (rec-fact (dec 2))))))


    ;; ;==(24)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (define n#2
    ;;   2)
    ;; (* 4 (* 3 (* 2 (rec-fact 1)))))


    ;; ;==(25)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (define n#2
    ;;   2)
    ;; (*
    ;;   4
    ;;   (*
    ;;   3
    ;;   (*
    ;;     2
    ;;     ((lambda (n)
    ;;       (if (<= n 0)
    ;;           1
    ;;           (* n (rec-fact (dec n)))))
    ;;     1)))))


    ;; ;==(26)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (define n#2
    ;;   2)
    ;; (define n#3
    ;;   1)
    ;; (*
    ;;   4
    ;;   (*
    ;;   3
    ;;   (*
    ;;     2
    ;;     (if (<= n#3 0)
    ;;         1
    ;;         (* n#3 (rec-fact (dec n#3))))))))


    ;; ;==(27)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (define n#2
    ;;   2)
    ;; (define n#3
    ;;   1)
    ;; (*
    ;;   4
    ;;   (*
    ;;   3
    ;;   (* 2
    ;;       (if (<= 1 0) 1 (* n#3 (rec-fact (dec n#3))))))))


    ;; ;==(28)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (define n#2
    ;;   2)
    ;; (define n#3
    ;;   1)
    ;; (*
    ;;   4
    ;;   (* 3 (* 2 (if #f 1 (* n#3 (rec-fact (dec n#3))))))))


    ;; ;==(29)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (define n#2
    ;;   2)
    ;; (define n#3
    ;;   1)
    ;; (* 4 (* 3 (* 2 (* n#3 (rec-fact (dec n#3)))))))


    ;; ;==(30)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (define n#2
    ;;   2)
    ;; (define n#3
    ;;   1)
    ;; (* 4 (* 3 (* 2 (* 1 (rec-fact (dec n#3)))))))


    ;; ;==(31)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (define n#2
    ;;   2)
    ;; (define n#3
    ;;   1)
    ;; (* 4 (* 3 (* 2 (* 1 (rec-fact (dec 1)))))))


    ;; ;==(32)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (define n#2
    ;;   2)
    ;; (define n#3
    ;;   1)
    ;; (* 4 (* 3 (* 2 (* 1 (rec-fact 0))))))


    ;; ;==(33)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (define n#2
    ;;   2)
    ;; (define n#3
    ;;   1)
    ;; (*
    ;;   4
    ;;   (*
    ;;   3
    ;;   (*
    ;;     2
    ;;     (*
    ;;     1
    ;;     ((lambda (n)
    ;;         (if (<= n 0)
    ;;             1
    ;;             (* n (rec-fact (dec n)))))
    ;;       0))))))


    ;; ;==(34)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (define n#2
    ;;   2)
    ;; (define n#3
    ;;   1)
    ;; (define n#4
    ;;   0)
    ;; (*
    ;;   4
    ;;   (*
    ;;   3
    ;;   (*
    ;;     2
    ;;     (*
    ;;     1
    ;;     (if (<= n#4 0)
    ;;         1
    ;;         (* n#4 (rec-fact (dec n#4)))))))))


    ;; ;==(35)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (define n#2
    ;;   2)
    ;; (define n#3
    ;;   1)
    ;; (define n#4
    ;;   0)
    ;; (*
    ;;   4
    ;;   (*
    ;;   3
    ;;   (*
    ;;     2
    ;;     (*
    ;;     1
    ;;     (if (<= 0 0)
    ;;         1
    ;;         (* n#4 (rec-fact (dec n#4)))))))))


    ;; ;==(36)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (define n#2
    ;;   2)
    ;; (define n#3
    ;;   1)
    ;; (define n#4
    ;;   0)
    ;; (*
    ;;   4
    ;;   (*
    ;;   3
    ;;   (* 2
    ;;       (* 1 (if #t 1 (* n#4 (rec-fact (dec n#4)))))))))


    ;; ;==(37)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (define n#2
    ;;   2)
    ;; (define n#3
    ;;   1)
    ;; (define n#4
    ;;   0)
    ;; (* 4 (* 3 (* 2 (* 1 1)))))


    ;; ;==(38)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (define n#2
    ;;   2)
    ;; (define n#3
    ;;   1)
    ;; (define n#4
    ;;   0)
    ;; (* 4 (* 3 (* 2 1))))


    ;; ;==(39)==>((define rec-fact
    ;;   (lambda (n)
    ;;     (if (<= n 0)
    ;;         1
    ;;         (* n (rec-fact (dec n))))))
    ;; (define n#0
    ;;   4)
    ;; (define n#1
    ;;   3)
    ;; (define n#2
    ;;   2)
    ;; (define n#3
    ;;   1)
    ;; (define n#4
    ;;   0)
    ;; (* 4 (* 3 2)))


    ;; ;==(40)==>(* 4 6)


    ;; ;==(41)==>24


    ;; ;==(42)==>24

    ;; Syntactic Value was returned
    ;; ;... done
    ;; ;Value: 24

;;; Eval 4! iteratively
(smeval
  '((define (iter-fact n)
  (define (iter n result)
    (if (<= n 0)
        result
        (iter (- n 1) (* n result))))
  (iter n 1))
    (iter-fact 4)))

;;; Output:
    ;; ;==(0)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (iter-fact 4))


    ;; ;==(1)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; ((lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1))
    ;;   4))


    ;; ;==(2)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (iter#1 n#0 1))


    ;; ;==(3)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (iter#1 4 1))


    ;; ;==(4)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; ((lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result))))
    ;;   4
    ;;   1))


    ;; ;==(5)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (if (<= n#1 0)
    ;;     result#0
    ;;     (iter#1 (- n#1 1) (* n#1 result#0))))


    ;; ;==(6)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (if (<= 4 0)
    ;;     result#0
    ;;     (iter#1 (- n#1 1) (* n#1 result#0))))


    ;; ;==(7)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (if #f
    ;;     result#0
    ;;     (iter#1 (- n#1 1) (* n#1 result#0))))


    ;; ;==(8)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (iter#1 (- n#1 1) (* n#1 result#0)))


    ;; ;==(9)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (iter#1 (- 4 1) (* n#1 result#0)))


    ;; ;==(10)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (iter#1 3 (* n#1 result#0)))


    ;; ;==(11)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (iter#1 3 (* 4 result#0)))


    ;; ;==(12)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (iter#1 3 (* 4 1)))


    ;; ;==(13)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (iter#1 3 4))


    ;; ;==(14)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; ((lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result))))
    ;;   3
    ;;   4))


    ;; ;==(15)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (if (<= n#2 0)
    ;;     result#1
    ;;     (iter#1 (- n#2 1) (* n#2 result#1))))


    ;; ;==(16)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (if (<= 3 0)
    ;;     result#1
    ;;     (iter#1 (- n#2 1) (* n#2 result#1))))


    ;; ;==(17)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (if #f
    ;;     result#1
    ;;     (iter#1 (- n#2 1) (* n#2 result#1))))


    ;; ;==(18)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (iter#1 (- n#2 1) (* n#2 result#1)))


    ;; ;==(19)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (iter#1 (- 3 1) (* n#2 result#1)))


    ;; ;==(20)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (iter#1 2 (* n#2 result#1)))


    ;; ;==(21)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (iter#1 2 (* 3 result#1)))


    ;; ;==(22)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (iter#1 2 (* 3 4)))


    ;; ;==(23)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (iter#1 2 12))


    ;; ;==(24)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; ((lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result))))
    ;;   2
    ;;   12))


    ;; ;==(25)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (define n#3
    ;;   2)
    ;; (define result#2
    ;;   12)
    ;; (if (<= n#3 0)
    ;;     result#2
    ;;     (iter#1 (- n#3 1) (* n#3 result#2))))


    ;; ;==(26)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (define n#3
    ;;   2)
    ;; (define result#2
    ;;   12)
    ;; (if (<= 2 0)
    ;;     result#2
    ;;     (iter#1 (- n#3 1) (* n#3 result#2))))


    ;; ;==(27)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (define n#3
    ;;   2)
    ;; (define result#2
    ;;   12)
    ;; (if #f
    ;;     result#2
    ;;     (iter#1 (- n#3 1) (* n#3 result#2))))


    ;; ;==(28)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (define n#3
    ;;   2)
    ;; (define result#2
    ;;   12)
    ;; (iter#1 (- n#3 1) (* n#3 result#2)))


    ;; ;==(29)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (define n#3
    ;;   2)
    ;; (define result#2
    ;;   12)
    ;; (iter#1 (- 2 1) (* n#3 result#2)))


    ;; ;==(30)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (define n#3
    ;;   2)
    ;; (define result#2
    ;;   12)
    ;; (iter#1 1 (* n#3 result#2)))


    ;; ;==(31)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (define n#3
    ;;   2)
    ;; (define result#2
    ;;   12)
    ;; (iter#1 1 (* 2 result#2)))


    ;; ;==(32)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (define n#3
    ;;   2)
    ;; (define result#2
    ;;   12)
    ;; (iter#1 1 (* 2 12)))


    ;; ;==(33)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (define n#3
    ;;   2)
    ;; (define result#2
    ;;   12)
    ;; (iter#1 1 24))


    ;; ;==(34)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (define n#3
    ;;   2)
    ;; (define result#2
    ;;   12)
    ;; ((lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result))))
    ;;   1
    ;;   24))


    ;; ;==(35)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (define n#3
    ;;   2)
    ;; (define result#2
    ;;   12)
    ;; (define n#4
    ;;   1)
    ;; (define result#3
    ;;   24)
    ;; (if (<= n#4 0)
    ;;     result#3
    ;;     (iter#1 (- n#4 1) (* n#4 result#3))))


    ;; ;==(36)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (define n#3
    ;;   2)
    ;; (define result#2
    ;;   12)
    ;; (define n#4
    ;;   1)
    ;; (define result#3
    ;;   24)
    ;; (if (<= 1 0)
    ;;     result#3
    ;;     (iter#1 (- n#4 1) (* n#4 result#3))))


    ;; ;==(37)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (define n#3
    ;;   2)
    ;; (define result#2
    ;;   12)
    ;; (define n#4
    ;;   1)
    ;; (define result#3
    ;;   24)
    ;; (if #f
    ;;     result#3
    ;;     (iter#1 (- n#4 1) (* n#4 result#3))))


    ;; ;==(38)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (define n#3
    ;;   2)
    ;; (define result#2
    ;;   12)
    ;; (define n#4
    ;;   1)
    ;; (define result#3
    ;;   24)
    ;; (iter#1 (- n#4 1) (* n#4 result#3)))


    ;; ;==(39)==>((define iter-fact
    ;;   (lambda (n)
    ;;     (define iter
    ;;       (lambda (n result)
    ;;         (if (<= n 0)
    ;;             result
    ;;             (iter (- n 1) (* n result)))))
    ;;     (iter n 1)))
    ;; (define n#0
    ;;   4)
    ;; (define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#1
    ;;   4)
    ;; (define result#0
    ;;   1)
    ;; (define n#2
    ;;   3)
    ;; (define result#1
    ;;   4)
    ;; (define n#3
    ;;   2)
    ;; (define result#2
    ;;   12)
    ;; (define n#4
    ;;   1)
    ;; (define result#3
    ;;   24)
    ;; (iter#1 (- 1 1) (* n#4 result#3)))


    ;; ;==(40)==>((define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#4
    ;;   1)
    ;; (define result#3
    ;;   24)
    ;; (iter#1 0 (* n#4 result#3)))


    ;; ;==(41)==>((define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#4
    ;;   1)
    ;; (define result#3
    ;;   24)
    ;; (iter#1 0 (* 1 result#3)))


    ;; ;==(42)==>((define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#4
    ;;   1)
    ;; (define result#3
    ;;   24)
    ;; (iter#1 0 (* 1 24)))


    ;; ;==(43)==>((define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#4
    ;;   1)
    ;; (define result#3
    ;;   24)
    ;; (iter#1 0 24))


    ;; ;==(44)==>((define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#4
    ;;   1)
    ;; (define result#3
    ;;   24)
    ;; ((lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result))))
    ;;   0
    ;;   24))


    ;; ;==(45)==>((define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#4
    ;;   1)
    ;; (define result#3
    ;;   24)
    ;; (define n#5
    ;;   0)
    ;; (define result#4
    ;;   24)
    ;; (if (<= n#5 0)
    ;;     result#4
    ;;     (iter#1 (- n#5 1) (* n#5 result#4))))


    ;; ;==(46)==>((define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#4
    ;;   1)
    ;; (define result#3
    ;;   24)
    ;; (define n#5
    ;;   0)
    ;; (define result#4
    ;;   24)
    ;; (if (<= 0 0)
    ;;     result#4
    ;;     (iter#1 (- n#5 1) (* n#5 result#4))))


    ;; ;==(47)==>((define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#4
    ;;   1)
    ;; (define result#3
    ;;   24)
    ;; (define n#5
    ;;   0)
    ;; (define result#4
    ;;   24)
    ;; (if #t
    ;;     result#4
    ;;     (iter#1 (- n#5 1) (* n#5 result#4))))


    ;; ;==(48)==>((define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#4
    ;;   1)
    ;; (define result#3
    ;;   24)
    ;; (define n#5
    ;;   0)
    ;; (define result#4
    ;;   24)
    ;; result#4)


    ;; ;==(49)==>((define iter#1
    ;;   (lambda (n result)
    ;;     (if (<= n 0)
    ;;         result
    ;;         (iter#1 (- n 1) (* n result)))))
    ;; (define n#4
    ;;   1)
    ;; (define result#3
    ;;   24)
    ;; (define n#5
    ;;   0)
    ;; (define result#4
    ;;   24)
    ;; 24)


    ;; ;==(50)==>24

    ;; Syntactic Value was returned
    ;; ;... done
    ;; ;Value: 24

;;==============================================================================
;;==============================================================================
