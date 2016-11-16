;;==============================================================================
;;     File: ex2_58.scm
;;  Created: 11/14/2016, 18:45
;;   Author: Bernie Roesler
;;
;;  Description: Modify differentiation for ordinary math notation (infix)
;;
;;==============================================================================
(load "deriv.scm")

;;; (a) assume + and * always have 2 arguments and fully parenthesized

;;; Test code:
; (printval (deriv '(x + (3 * (x + (y + 2)))) 'x))
;;==============================================================================
;;==============================================================================
