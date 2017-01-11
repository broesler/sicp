;;==============================================================================
;;     File: ex3_6.scm
;;  Created: 01/11/2017, 17:14
;;   Author: Bernie Roesler
;;
;;  Description: Redefine rand to take a reset argument
;;
;;==============================================================================
(define random-init 7)
(load "../../sicp_code/ch3support.scm") ; rand-update

;;; Redefine rand to take arguments 'reset or 'generate
(define rand
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate)
             (set! x (rand-update x))
             x)
            ((eq? m 'reset)
             (lambda (v) (set! x v) #t))
            (else (error "No procedure by this name -- RAND" m))))))

;;; Test code:
(printval (rand 'generate))  ; Value: 88
(printval ((rand 'reset) 9)) ; Value: #t
(printval (rand 'generate))  ; Value: 15
(printval (rand 'generate))  ; Value: 50
(printval ((rand 'reset) 9)) ; Value: #t
(printval (rand 'generate))  ; Value: 15
(printval (rand 'generate))  ; Value: 50

;;==============================================================================
;;==============================================================================
