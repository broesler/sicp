;;==============================================================================
;;     File: ex2_85.scm
;;  Created: 12/31/2016, 18:20
;;   Author: Bernie Roesler
;;
;;  Description: Define drop procedure to simplify answers
;;
;;==============================================================================
(load "ex2_84.scm") ; loads (apply-generic)

;------------------------------------------------------------------------------- 
;        Project one level
;-------------------------------------------------------------------------------
;;; Push a number down the tower (install in each type)
(define (project n) (apply-generic 'project n))

;;; Test code:
;; values defined in ex2_84.scm
; (newline)
; (display "Projecting...")
; (printval (project a)) ; Value: 7
; (printval (project b)) ; Value: 1
; (printval (project c)) ; Value: (rational 4 . 1)
; (printval (project d)) ; Value: 1.

;------------------------------------------------------------------------------- 
;        Drop as far as possible
;-------------------------------------------------------------------------------
;;; If a number can be projected, then raised back up, and is equal to the
;;; original number, then we continue to drop. Otherwise, just return the number
(define (drop n)
  (let* ((p (project n))
         (rp (raise p)))
    (if (and (not (equal? (type-tag n) (type-tag p)))
             (equ? rp n)) 
      (drop p)
      n)))

;;; Test (drop)
; (newline)
; (display "3..2..1..dropping!")
; (printval (drop (make-complex-from-real-imag 2   3))) ; Value: (complex rectangular 2 . 3)
; (printval (drop (make-complex-from-real-imag 1.5 0))) ; Value: 1.5
; (printval (drop (make-complex-from-real-imag 1   0))) ; Value: 1

;------------------------------------------------------------------------------- 
;        Redefine apply-generic using "drop"
;-------------------------------------------------------------------------------
(define (apply-generic op . args)
  ;; Raising to the prespecified type 
  ;; NOTE: this procedure will go into an infinite loop in case the given type
  ;; is lower than argument's, but it works fine as a part of apply-generic
  ;; procedure because we enforce proper usage
  (define (raise-to arg type) 
    (let ((this-tag (type-tag arg))) 
      (if (eq? this-tag type) 
        arg 
        (raise-to (raise arg) type)))) 

  ;; Get higher type from 2 arguments
  (define (pick-higher arg1 arg2) 
    (define (find-iter arg1 arg2 result) 
      (let ((tag1 (type-tag arg1)) 
            (tag2 (type-tag arg2)) 
            (move-a1 (raise arg1))) 
        (let ((next-tag1 (type-tag move-a1))) 
          (cond ((eq? tag1 tag2) arg2) ; arg2 > arg1, but we've moved arg1 up
                ((eq? tag1 next-tag1) result) ; we've hit the top of the tower
                (else (find-iter move-a1 arg2 result)))))) 
    (find-iter arg1 arg2 arg1)) ; start with arg1 in case arg1 > arg2

  ;; Pick the highest type argument from the list 
  (define (find-highest myargs) 
    (if (null? (cdr myargs)) 
      (car myargs) 
      (let ((this (car myargs)) 
            (next (cadr myargs)) 
            (rest (cddr myargs))) 
        (let ((t1 (type-tag this)) 
              (t2 (type-tag next))) 
          (if (eq? t1 t2) 
            (find-highest (cdr myargs)) 
            (find-highest (cons (pick-higher this next) 
                                rest))))))) 

  ;; Raise the entire argument list to the type (accumulate operation)
  (define (raise-all-to-highest myargs type) 
    (if (null? myargs) 
      '() 
      (cons (raise-to (car myargs) type) 
            (raise-all-to-highest (cdr myargs) type))))

  ;; Main procedure
  (let ((type-tags (map type-tag args))) 
    (let ((proc (get op type-tags))) 
      (if proc 
        ;; Need to check if result is "droppable" or not... a bit awkward for
        ;; adding new procedures! We need to check the op because "project" was
        ;; defined using apply-generic as well... so we'll get an infinite loop
        ;; if we try to drop the result of a projection!
        (let ((result (apply proc (map contents args))))
          (if (or (eq? op 'add) 
                  (eq? op 'sub) 
                  (eq? op 'mul)
                  (eq? op 'div))
            ; (drop result)
            result
            result))
        (if (> (length args) 1) 
          (let ((t1 (car type-tags)) 
                (t2 (cadr type-tags)) 
                (rest-args (cddr args))) 
            (if (and (null? rest-args) (eq? t1 t2)) 
              (error "No procedure specified for these types" op) 
              (let ((highest-type (type-tag (find-highest args)))) 
                (let ((raised-args (raise-all-to-highest args highest-type))) 
                  (apply apply-generic op raised-args))))) 
          (error "No procedure specified for this type" (list op type-tags)))))))

;;; Test code:
; (newline)
; (display "drop apply-generic:")
; (printval (add a b)) ; Value: (rational 31 . 4)
; (printval (add a c)) ; Value: (rational 21 . 2)
; (printval (add d b)) ; Value: (complex rectangular 1.75 . 1.)
; (printval (add d a)) ; Value: (complex rectangular 8. . 1.)
;;; Simplified answers:
; (printval (add (make-rational 1 1) a)) ; Value: 8
; (printval (add (make-complex-from-real-imag 1 0) a)) ; Value: 8
;;==============================================================================
;;==============================================================================
