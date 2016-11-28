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
(define (get-record employee division)
  ((get 'get-record division) employee))

;;; each division's file must supply a (get-record) procedure to return the
;;; requested employee data, and tag that procedure with the division ID

;------------------------------------------------------------------------------- 
;        (b) get salary from employee record
;-------------------------------------------------------------------------------
;;; Divisions must have a procedure (get-salary) that operates on an individual
;;; employee record, and is tagged with the division ID.
(define (get-salary employee division)
  (let ((emp ((get 'get-record division) employee)))
    ((get 'get-salary division) emp)))

;------------------------------------------------------------------------------- 
;        (c) find employeed record in all divisions
;-------------------------------------------------------------------------------
;;; Divisions must have a predicate that returns true if an employee name
;;; appears in their record
(define (find-employee-record employee divisions)
  (cond ((null? divisions)
         false)
        ((employee-in-division? employee (car divisions))
         (get-record employee (car divisions)))
        (else (find-employee-record employee (cdr divisions)))))

(define (employee-in-division? employee division)
  ((get 'employee-in-division? division) employee))

;;==============================================================================
;;==============================================================================
