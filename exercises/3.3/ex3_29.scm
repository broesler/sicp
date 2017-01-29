;;==============================================================================
;;     File: ex3_29.scm
;;  Created: 01/18/2017, 16:40
;;   Author: Bernie Roesler
;;
;;  Description: or-gate from and-gates and inverters
;;
;;==============================================================================

;;; Define or gate
(define (or-gate o1 o2 output)
  (let ((a (make-wire))
        (b (make-wire))
        (c (make-wire)))
    (inverter o1 a)
    (inverter o2 b)
    (and-gate a b c)
    (inverter c output))
  'ok)

;;; propagation delay = inverter + and + inverter
;;;                   = 2*inverter-delay + and-gate-delay

;;==============================================================================
;;==============================================================================
