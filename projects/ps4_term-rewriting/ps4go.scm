;;==============================================================================
;;     File: ps4go.scm
;;  Created: 11/22/2016, 15:12
;;   Author: Bernie Roesler
;;
;;  Description: Tests of given source code
;;
;;==============================================================================

;;; Load problem set code
(load "smfresh.scm")
(load "smsyntax.scm")
(load "smscope.scm")
(load "smstep.scm")
(load "smeval.scm")

;;; Test functions:
;; Simple addition
; (smeval '((lambda (n) (+ 2 n)) 3))

;; Factorial prints a lot of steps...
; (smeval
;   '((define (rec-factorial n)
;       (if (<= n 0)
;         1
;         (* n (rec-factorial (dec n)))))
;     (rec-factorial 3)))

;;; Only print final value:
; (define reversed-steps 
;   (smstep-list 
;     '((define (rec-fact n)
;         (if (<= n 0)
;           1
;           (* n (rec-fact (dec n)))))
;       (rec-fact 4))))
; (define final-body (body-or-info-of-step (cadr reversed-steps)))
; (printval (printable-version final-body))
;==============================================================================
;==============================================================================
