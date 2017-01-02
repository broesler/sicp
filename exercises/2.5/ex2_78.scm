;;==============================================================================
;;     File: ex2_78.scm
;;  Created: 12/14/2016, 23:48
;;   Author: Bernie Roesler
;;
;;  Description: Modify tagging for regular numbers
;;
;;==============================================================================
(load "generic_arithmetic.scm")

;;; Modify the definitions of type-tag, contents, and attach-tag from section
;;; 2.4.2 so that our generic system takes advantage of Scheme's internal type
;;; system. That is to say, the system should work as before except that
;;; ordinary numbers should be represented simply as Scheme numbers rather than
;;; as pairs whose car is the symbol scheme-number.

;;; Tagging data 
(define (attach-tag type-tag contents)
  (if (not (eq? type-tag 'scheme-number))
    (cons type-tag contents)
    contents)) ; for a regular number, just return the number (no tag)

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number) ; pretend we have a tag
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum) ; number is not a pair
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

;;; Test code:
(define a (make-scheme-number 3))
(define b (make-scheme-number 4))
(add a b)
;;==============================================================================
;;==============================================================================
