;;==============================================================================
;;     File: ex3_3.scm
;;  Created: 01/11/2017, 10:54
;;   Author: Bernie Roesler
;;
;;  Description: password-protected make-account
;;
;;==============================================================================

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  ;; Define a procedure that takes an argument (like withdraw/deposit) so we
  ;; don't have to throw an error
  (define (wrong-password m)
      "Incorrect password")
  (define (dispatch p m)
    (if (eq? p password)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                        m)))
      wrong-password))
  dispatch)

;;; Test code:
(define acc (make-account 100 'secret-password))
(printval ((acc 'secret-password 'withdraw) 40)) ; Value: 60
(printval ((acc 'some-other-password 'deposit) 50)) 
; "Incorrect password -- MAKE-ACCOUNT"

;;==============================================================================
;;==============================================================================
