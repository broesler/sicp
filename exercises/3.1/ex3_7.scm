;;==============================================================================
;;     File: ex3_7.scm
;;  Created: 01/11/2017, 17:51
;;   Author: Bernie Roesler
;;
;;  Description: Joint bank accounts
;;
;;==============================================================================

;;; Define a joint account
(define (make-joint acct old-pwd new-pwd)
  (lambda (p m)
    (if (eq? p new-pwd)
      (acct old-pwd m)
      (lambda (m) "Incorrect password"))))

;;; Define account with a password
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
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
(define peter-acc (make-account 100 'open-sesame))
(printval ((peter-acc 'open-sesame 'withdraw) 40)) ; Value: 60
(printval ((peter-acc 'apostle 'deposit) 50)) ; Value:  "Incorrect password"

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))
(printval ((paul-acc 'rosebud 'withdraw) 25)) ; Value: 35
(printval ((paul-acc 'sonofgod 'withdraw) 25)) ; Value:  "Incorrect password"

;;==============================================================================
;;==============================================================================
