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
  (lambda w
    (define w-even? (even? (car w)))
    (define (iter a result odd-even?)
      (cond ((null? a) 
             result)
            (else 
              (let ((x (cond (odd-even? 
                               (if (even? (car a))
                                 (append result (list (car a)))
                                 result))
                             (else
                               (if (even? (car a))
                                 result
                                 (append result (list (car a))))))))
                (iter (cdr a) x odd-even?)))))
    (iter w '() w-even?)))

;; Test code:
(same-parity 1 2 3 4 5 6 7)  ; Value: (1 3 5 7)
(same-parity 2 3 4 5 6 7)    ; Value: (2 4 6)
;;==============================================================================
;;==============================================================================
