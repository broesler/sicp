;;==============================================================================
;;     File: ex2_20.scm
;;  Created: 11/10/2016, 00:06
;;   Author: Bernie Roesler
;;
;;  Description: dotted-tail notation 
;;
;;==============================================================================

; Use this notation to write a procedure same-parity that takes one or more
; integers and returns a list of all the arguments that have the same even-odd
; parity as the first argument. For example,
;   (same-parity 1 2 3 4 5 6 7) 
;   (1 3 5 7)
;   (same-parity 2 3 4 5 6 7) 
;   (2 4 6)

(define same-parity 
  (lambda (z . w)
    (let ((w-even (remainder z 2)))
      (define (iter a result)
        (cond ((null? a) 
               (reverse result))
              (else (iter 
                      (cdr a) 
                      (if (= (remainder (car a) 2) w-even)
                        (cons (car a) result)
                        result)))))
      (iter w (cons z '())))))

;; Test code:
(printval (same-parity 1 2 3 4 5 6 7))  ; Value: (1 3 5 7)
(printval (same-parity 2 3 4 5 6 7))    ; Value: (2 4 6)
;;==============================================================================
;;==============================================================================
