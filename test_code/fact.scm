;;==============================================================================
;;     File: fact.scm
;;  Created: 10/27/2016, 23:36
;;   Author: Bernie Roesler
;;
;;  Description: 
;;
;;==============================================================================

; Linear recursive definition
(define ! 
  (lambda (n)
    (if (= n 0)
      1
      (* n (! (- n 1)))
      )
    )
  )

; Linear iterative definition (product from 1 to n)
(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* counter product)
               (+ counter 1)
               max-count)))

; Rewrite with internal definition 
(define (factorial n)
  (define (iter product counter) 
    (if (> counter n)
      product
      (iter (* counter product)
            (+ counter 1))))
  (iter 1 1))
;;==============================================================================
;;==============================================================================
