;;==============================================================================
;;     File: ex3_35.scm
;;  Created: 02/03/2017, 10:35
;;   Author: Bernie Roesler
;;
;;  Description: squarer as a primitive 
;;
;;==============================================================================
(load "constraints.scm")

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
      (if (< (get-value b) 0)
        (error "square less than 0 -- SQUARER" (get-value b))
        (set-value! a 
                    (sqrt (get-value b)) 
                    me))
      (if (has-value? a)
        (set-value! b 
                    (square (get-value a)) 
                    me))))
  (define (process-forget-value) 
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
            (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

;;; Test code:
(define a (make-connector))
(define b (make-connector))
(probe "a" a)
(probe "b" b)
(squarer a b)
(set-value! a 2 'bernie)
; Value: Probe: b = 4.
; Value: Probe: a = 2.
(forget-value! a 'bernie)
; Value: Probe: a = ?
; Value: Probe: b = ?
(set-value! b 2.0 'bernie)
; Value: Probe: a = 1.4142135623730951
; Value: Probe: b = 2

;;==============================================================================
;;==============================================================================
