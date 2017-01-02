;;==============================================================================
;;     File: ex2_23.scm
;;  Created: 11/10/2016, 01:33
;;   Author: Bernie Roesler
;;
;;  Description: Implement for-each (map without returning a list) 
;;
;;==============================================================================

;; takes a function of 1 argument and a list
(define (my-for-each f items)
  (cond ((null? items) "done")
        (else (f (car items)) 
              (my-for-each f (cdr items)))))

;; Test code:
(define x (list 1 2 3 4))
(my-for-each (lambda (x) (newline) (display x)) x)

;;==============================================================================
;;==============================================================================
