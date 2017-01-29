;;==============================================================================
;;     File: ex3_30.scm
;;  Created: 01/18/2017, 16:50
;;   Author: Bernie Roesler
;;
;;  Description: ripple-carry adder
;;
;;==============================================================================

;;; Ripple-carry adder
;;; A,B are n-bit inputs (lists where [AB]1 is MSB, [AB]n is LSB)
;;; S is n-bit sum output (list ordered as above)
;;; C is 1-bit carry output
(define (ripple-carry-adder A B S C)
  (let ((ck (make-wire)))  ; internal wire ("carry out")
    (cond ((null? (cdr A)) ; assume A,B,S are all proper length
           (let ((c-in (make-wire)))
             (set-signal! c-in 0)
             (full-adder (car A) (car B) c-in (car S) C)))
          (else
            (full-adder (car A)
                        (car B)
                        ck
                        (car S)
                        C)
            (ripple-carry-adder (cdr A) (cdr B) (cdr S) ck)))))

;;; ripple-carry-delay = (n-1)*(full-adder-delay)
;;;                    = (n-1)*(2*half-adder-delay + or-gate-delay)
;;;                    = (n-1)*(2*and-gate-delay + or-gate-delay)
;;;
;;; NOTE carry bit is the propagating signal. It depends on "B" to propagate
;;; through two half-adders (first step), which each require that "B" pass
;;; through just one and-gate to produce the carry bit (second step).
;;==============================================================================
;;==============================================================================
