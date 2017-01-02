;;==============================================================================
;;     File: ex2_84.scm
;;  Created: 12/23/2016, 18:39
;;   Author: Bernie Roesler
;;
;;  Description: Redefine apply-generic for use with (raise)
;;
;;==============================================================================
(load "ex2_83.scm") ; loads (raise) and generic_arithmetic.scm

;;; (a) apply-generic with arbitrary number of arguments
(define (apply-generic op . args)
  ;; Raising to the prespecified type (the issue is known that it will go into
  ;; an infinite loop in case the type is lower than argument's. but still it
  ;; works fine as a part of apply-generic procedure) 
  (define (raise-to arg type) 
    (let ((this-tag (type-tag arg))) 
      (if (eq? this-tag type) 
        arg 
        (raise-to (raise arg) type)))) 

  ;; Testing 2 arguments which has the highest type 
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

  ;; Picking the highest type argument from the list 
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

  ;; Raising the entire argument list to the type (accumulate operation)
  (define (raise-all-to-highest myargs type) 
    (if (null? myargs) 
      '() 
      (cons (raise-to (car myargs) type) 
            (raise-all-to-highest (cdr myargs) type))))

  ;; Special procedure to partition our argument list in pieces of two, since
  ;; our packages arithmetic procedures were specified for maximum of
  ;; 2 arguments, we'll do it this way: 
  (define (partition-and-apply op myargs) 
    (if (null? (cdr myargs)) 
      (car myargs) 
      (let ((a1 (car myargs)) 
            (a2 (cadr myargs)) 
            (rest-args (cddr myargs))) 
        (partition-and-apply op (cons (apply-generic op a1 a2) rest-args))))) 

  ;; Main procedure
  (let ((type-tags (map type-tag args))) 
    (let ((proc (get op type-tags))) 
      (if proc 
        (apply proc (map contents args)) 
        (if (> (length args) 1) 
          (let ((t1 (car type-tags)) 
                (t2 (cadr type-tags)) 
                (rest-args (cddr args))) 
            (if (and (null? rest-args) (eq? t1 t2)) 
              (error "No procedure specified for these types" op) 
              (let ((highest-type (type-tag (find-highest args)))) 
                (let ((raised-args (raise-all-to-highest args highest-type))) 
                  (partition-and-apply op raised-args))))) 
          (error "No procedure specified for this type" (list op type-tags)))))))

;;; Test code:
(define a (make-scheme-number 7))
(define b (make-rational 3 4))
(define c (make-real 3.5))
(define d (make-complex-from-real-imag 1 1))
; (printval (add a b)) ; Value: (rational 31 . 4)
; (printval (add a c)) ; Value: 10.5
; (printval (add d b)) ; Value: (complex rectangular 1.75 . 1.)
; (printval (add d a)) ; Value: (complex rectangular 8. . 1.)
;;; Unsimplified answers:
; (printval (add (make-rational 1 1) a)) ; Value: (rational 8 . 1)
; (printval (add (make-complex-from-real-imag 1 0) a))
; Value: (complex rectangular 8. . 0.)

;;; NOTE: The following does not work without redefining (add)
; (printval (add a b c d))
;;; This works though:
; (printval (apply-generic 'add a b c d)) 
; Value: (complex rectangular 12.25 . 1)

;;==============================================================================
;;==============================================================================
