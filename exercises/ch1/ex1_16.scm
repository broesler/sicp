;;==============================================================================
;;     File: ex1_16.scm
;;  Created: 11/02/2016, 14:28
;;   Author: Bernie Roesler
;;
;;  Description: Exponentiation
;;
;;==============================================================================

; Linear recursive
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

; Linear iterative
; (define (expt b n)
;   (expt-iter b n 1))
;
; (define (expt-iter b counter product)
;   (if (= counter 0)
;       product
;       (expt-iter b
;                 (- counter 1)
;                 (* b product))))

; Successive squaring recursive
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; primitive remainder
(define (even? n)
  (= (remainder n 2) 0))


; Successive squaring iterative
; Keep invariant quantity ab^n through the process
(define (fast-expt-i b n)
  (fast-expt-iter 1 b n))

(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
        ; if even, just keep squaring b, and divide n by 2
        ((even? n) (fast-expt-iter a (square b) (/ n 2)))
        ; if odd, "accumulate" b into a, keep b, and only subtract one from n
        (else (fast-expt-iter (* a b) b (- n 1)))))

; Test code
(expt 2.0 1000)         ;Value: 1.0715086071862673e301
(fast-expt 2.0 1000)    ;Value: 1.0715086071862673e301
(fast-expt-i 2.0 1000)  ;Value: 1.0715086071862673e301
;;==============================================================================
;;==============================================================================
