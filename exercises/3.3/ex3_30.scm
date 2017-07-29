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

;;; Test Code:
(load "circuit_simulation.scm") ; circuit primitives

(define A1 (make-wire)) ; input-1 (3-bit number)
(define A2 (make-wire))
(define A3 (make-wire))
(define B1 (make-wire)) ; input-2 (3-bit number)
(define B2 (make-wire))
(define B3 (make-wire))
(define S1 (make-wire)) ; sum (3-bit number)
(define S2 (make-wire))
(define S3 (make-wire))
(define C  (make-wire)) ; carry bit

(define A (list A1 A2 A3))
(define B (list B1 B2 B3))
(define S (list S1 S2 S3))

(probe 'sum1 S1)
(probe 'sum2 S2)
(probe 'sum3 S3)
(probe 'carry C)

(ripple-carry-adder A B S C)

;; Add 101 + 001 = 110 (i.e. 5 + 1 = 6)
(set-signal! A1 1)
(set-signal! A2 0)
(set-signal! A3 1)
(set-signal! B1 0)
(set-signal! B2 0)
(set-signal! B3 1)
(propagate)
;;==============================================================================
;;==============================================================================
