;;==============================================================================
;;     File: complex.scm
;;  Created: 12/13/2016, 23:06
;;   Author: Bernie Roesler
;;
;;  Description: Complex numbers generic system Section 2.4.3
;;
;;==============================================================================
(load "../../../sicp_code/ch2support.scm") ;; Include put/get operations

;;; Top-level operations
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

;;; Tagging data (Section 2.4.2)
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;------------------------------------------------------------------------------- 
;        Rectangular and Polar packages (to be used in 2.5)
;-------------------------------------------------------------------------------
;;; Ben's implementation
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (add (square (real-part z))     ; use generic operation
               (square (imag-part z)))))
  (define (angle z)
    (arctan (div (imag-part z) (real-part z))))
  (define (make-from-mag-ang r a) 
    (cons (mul r (cose a)) (mul r (sine a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'c-real-part '(rectangular) real-part)
  (put 'c-imag-part '(rectangular) imag-part)
  (put 'c-magnitude '(rectangular) magnitude)
  (put 'c-angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;;; Alyssa's implementation
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cose (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (add (square x) (square y)))
          (arctan (div y x))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'c-real-part '(polar) real-part)
  (put 'c-imag-part '(polar) imag-part)
  (put 'c-magnitude '(polar) magnitude)
  (put 'c-angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;;; Generic procedures
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

;;; Selectors
(define (c-real-part z) (apply-generic 'c-real-part z))
(define (c-imag-part z) (apply-generic 'c-imag-part z))
(define (c-magnitude z) (apply-generic 'c-magnitude z))
(define (c-angle z) (apply-generic 'c-angle z))

;;; Constructors
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;;; Install packages:
(install-rectangular-package)
(install-polar-package)
;;==============================================================================
;;==============================================================================
