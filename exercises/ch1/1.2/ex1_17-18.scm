;;==============================================================================
;;     File: ex1_17-18.scm
;;  Created: 11/02/2016, 22:19
;;   Author: Bernie Roesler
;;
;;  Description: Repeated-addition multiplication
;;
;;==============================================================================

; redefine multiply via repeated addition (linear in b)
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

; primitive remainder
(define (even? n)
  (= (remainder n 2) 0))

;------------------------------------------------------------------------------ 
;        Ex 1.16 fast multiply (log b time!)
;------------------------------------------------------------------------------
(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (- b 1))))))

; "primitives"
(define (double x)
  (* x 2))
(define (halve x)
  (if (even? x)
    (/ x 2)
    (error x "is not even!")))

; Notes:
; (fast-mult 3 8)     ; == 3 + 3 + 3 + 3 + 3 + 3 + 3 + 3 ;Value: 24
; (double (fast-mult 3 4))
; (double (double (fast-mult 3 2)))
; (double (double (double (fast-mult 3 1))))
; (double (double (double (+ 3 (fast-mult 3 0)))))
; (double (double (double (+ 3 0))))
; (double (double (double 3)))
; (double (double 6))
; (double 12)
;Value: 24

; Test code:
(fast-mult 3 8)  ;Value: 24

;------------------------------------------------------------------------------ 
;       Ex 1.17 
;------------------------------------------------------------------------------
(define (fast-mult-i a b)
  (fast-mult-iter 0 a b))

(define (fast-mult-iter t a b)
  (cond ((= b 0) t)
        ((even? b) (fast-mult-iter t (double a) (halve b)))
        (else (fast-mult-iter (+ t a) a (- b 1)))))

; Test code:
(fast-mult-i 3 8)  ;Value: 24

;;==============================================================================
;;==============================================================================
