;;==============================================================================
;;     File: ex2_74.scm
;;  Created: 11/27/2016, 17:40
;;   Author: Bernie Roesler
;;
;;  Description: Insatiable Enterprises record system
;;
;;==============================================================================

;;; As an example, suppose that each division's personnel records consist of
;;; a single file, which contains a set of records keyed on employees' names.
;;; The structure of the set varies from division to division. Furthermore, each
;;; employee's record is itself a set (structured differently from division to
;;; division) that contains information keyed under identifiers such as address
;;; and salary.

;------------------------------------------------------------------------------- 
;        (a) get employee record from personnel file
;-------------------------------------------------------------------------------
(define (get-record employee personnel)
  ((get 


;;==============================================================================
;;==============================================================================
