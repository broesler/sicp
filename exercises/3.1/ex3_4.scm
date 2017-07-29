;;==============================================================================
;;     File: ex3_4.scm
;;  Created: 01/11/2017, 10:59
;;   Author: Bernie Roesler
;;
;;  Description: password-protected make-account with repetition count
;;
;;==============================================================================

(define (make-account balance password)
  (define wrong-password-count 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (wrong-password m)
    (set! wrong-password-count (1+ wrong-password-count))
    (if (> wrong-password-count 7)
      (call-the-cops)
      "Incorrect password"))
  (define (call-the-cops)
    "Entered wrong password too many times -- Calling the cops!")
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
(printval ((acc 'itb56 'deposit) 50)) ; Value: "Incorrect password"
(printval ((acc 'itb56 'deposit) 50)) ; Value: "Incorrect password"
(printval ((acc 'itb56 'deposit) 50)) ; Value: "Incorrect password"
(printval ((acc 'itb56 'deposit) 50)) ; Value: "Incorrect password"
(printval ((acc 'itb56 'deposit) 50)) ; Value: "Incorrect password"
(printval ((acc 'itb56 'deposit) 50)) ; Value: "Incorrect password"
(printval ((acc 'itb56 'deposit) 50)) ; Value: "Incorrect password"
(printval ((acc 'itb56 'deposit) 50)) 
; Value: Entered wrong password too many times -- Calling the cops!

;;==============================================================================
;;==============================================================================
