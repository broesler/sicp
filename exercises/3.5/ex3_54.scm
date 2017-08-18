;;==============================================================================
;;     File: ex3_54.scm
;;  Created: 08/17/2017, 21:28
;;   Author: Bernie Roesler
;;
;;  Description: mul-streams and factorial
;;
;;==============================================================================
(load "streams.scm")

(define (mul-streams s1 s2)
  (stream-map * s1 s2))


(define factorials 
  (cons-stream 1 
               (mul-streams (add-streams ones integers) ; n+1
                            factorials)))               ; accumulate product

;;; Test code
(printval (stream-ref factorials 0)) ; (0+1)! =  1  ; Value:  1
(printval (stream-ref factorials 1)) ; (1+1)! =  2  ; Value:  2
(printval (stream-ref factorials 2)) ; (2+1)! =  6  ; Value:  6
(printval (stream-ref factorials 4)) ; (4+1)! = 24  ; Value: 24

;;==============================================================================
;;==============================================================================
