;;==============================================================================
;;     File: ex3_10.scm
;;  Created: 01/14/2017, 12:01
;;   Author: Bernie Roesler
;;
;;  Description: Test make-withdraw alternate definition
;;
;;==============================================================================

;;; Define local state variable explicitly using let:
(define (make-withdraw init)
  (let ((balance init))
    (lambda (amt)
      (if (>= balance amt)
        (begin (set! balance (- balance amt))
               balance)
        "Insufficient funds!"))))

;;; Rewrite as lambda for clarity:
(define (make-withdraw init)
  ((lambda (balance)
    (lambda (amt)
      (if (>= balance amt)
        (begin (set! balance (- balance amt))
               balance)
        "Insufficient funds!")))
  init))

(define W1 (make-withdraw 100))
(printval (W1 50))
(define W2 (make-withdraw 100))
;;==============================================================================
;;==============================================================================
