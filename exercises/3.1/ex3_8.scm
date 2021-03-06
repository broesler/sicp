;;==============================================================================
;;     File: ex3_8.scm
;;  Created: 01/11/2017, 18:04
;;   Author: Bernie Roesler
;;
;;  Description: Order of operations
;;
;;==============================================================================

(define f
  (let ((state #f))
    (lambda (x)
      (if state
        0
        (begin (set! state #t) 
               x)))))

;;; Test code:
(printval (+ (f 0) (f 1))) ; Value: 1
(printval (+ (f 1) (f 0))) ; Value: 0
;;==============================================================================
;;==============================================================================
