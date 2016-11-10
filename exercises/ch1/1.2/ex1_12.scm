;;==============================================================================
;;     File: ex1_12.scm
;;  Created: 11/01/2016, 17:46
;;   Author: Bernie Roesler
;;
;;  Description: Pascal's triangle
;;
;;==============================================================================

; n >= k >= 0
(define (pascal n k)
  (cond ((or (< n 0) (< k 0))
         (error "n =" n ", and k =" k "must be >= 0"))
        ((> k n)
         (error "k =" k ", must be <= n =" n "!"))
        ((or (= n 0) (= k 0) (= k n)) 
         1)
        (else (+ (pascal (- n 1) (- k 1))
                 (pascal (- n 1) k)))))


;;==============================================================================
;;==============================================================================