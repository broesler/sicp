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

;;; Tagging data
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

;;; Ben's implementation
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
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
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
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
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;;; Constructors
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;------------------------------------------------------------------------------- 
;        Test code from sicp_code/ch2tests.scm
;-------------------------------------------------------------------------------
;;; Load packages
(install-rectangular-package)
(install-polar-package)

;;; SECTION 2.4.2

(define z1 (make-from-real-imag 1 1)) ;Value: z1

z1 ;Value 20: (rectangular 1 . 1)
(real-part z1) ;Value: 1
(imag-part z1) ;Value: 1
(magnitude z1) ;Value: 1.4142135623730951
(angle z1) ;Value: .7853981633974483

(define z2 (make-from-mag-ang 1.4142135623730951 .7853981633974483)) ;Value: z2

z2 ;Value 22: (polar 1.4142135623730951 . .7853981633974483)

(magnitude z2) ;Value: 1.4142135623730951
(angle z2) ;Value: .7853981633974483
(real-part z2) ;Value: 1.
(imag-part z2) ;Value: 1.

z1 ;Value 20: (rectangular 1 . 1)
z2 ;Value 22: (polar 1.4142135623730951 . .7853981633974483)

(add-complex z1 z2) ;Value 23: (rectangular 2. . 2.)
(sub-complex z1 z2) ;Value 24: (rectangular 0. . 0.)
(mul-complex z1 z2) ;Value 25: (polar 2.0000000000000004 . 1.5707963267948966)
(div-complex z1 z2) ;Value 26: (polar 1. . 0.)

;;==============================================================================
;;==============================================================================
