;;==============================================================================
;;     File: ex2_85.scm
;;  Created: 12/31/2016, 18:20
;;   Author: Bernie Roesler
;;
;;  Description: Define drop procedure to simplify answers
;;
;;==============================================================================
(load "ex2_84.scm") ; loads (apply-generic)

;;; Push a number down the tower (install in each type)
(define (project n) (apply-generic 'project n))

;;; If a number can be projected, then raised back up, and is equal to the
;;; original number, then we continue to drop. Otherwise, just return the number
(define (drop n)
  (let* ((p (project n))
         (rp (raise p)))
    (if (equ? rp n)
      ;; Can drop if don't have the same type-tag already (i.e. hit bottom)
      (if (equal? (type-tag n) (type-tag p))
        n
        (drop p))
      n)))

;;; Test code:
;; values defined in ex2_84.scm
(printval (project a)) ; Value: 7
(printval (project b)) ; Value: 3
(printval (project c)) ; Value: (rational 3. . 1.)
(printval (project d)) ; Value: 1.

;;; Test (drop)
(newline)
(display "3..2..1..dropping!")
(printval (drop (make-complex-from-real-imag 2   3))) ; Value: (complex rectangular 2 . 3)
(printval (drop (make-complex-from-real-imag 1.5 0))) ; Value: 1.5
(printval (drop (make-complex-from-real-imag 1   0))) ; Value: 1
;;==============================================================================
;;==============================================================================
