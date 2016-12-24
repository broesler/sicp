;;==============================================================================
;;     File: generic_arithmetic.scm
;;  Created: 12/13/2016, 22:46
;;   Author: Bernie Roesler
;;
;;  Description: Generic arithmetic package from SICP Section 2.5 
;;
;;==============================================================================
(load "../2.4/complex.scm")
;;; loads 
;;;   -- internal complex procedures (rectangular/polar)
;;;   -- put/get, put/get-coercion
;;;   -- apply-generic
;;;   -- original tagging procedures

;------------------------------------------------------------------------------- 
;        Operations
;-------------------------------------------------------------------------------
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y)) ; Ex 2.79
(define (=zero? x) (apply-generic '=zero? x)) ; Ex 2.80
(define (raise n) (apply-generic 'raise n)) ; Ex 2.83

;------------------------------------------------------------------------------- 
;        Ex 2.78: Redefine tagging data
;-------------------------------------------------------------------------------
;;; Redefine tagging procedures to recognize scheme-numbers as regular numbers
(define (attach-tag type-tag contents)
  (if (number? contents)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum)    ; pretend we have a tag
         (if (exact? datum) 
           'scheme-number 
           'real))
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum) ; number is not a pair
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

;------------------------------------------------------------------------------- 
;        Scheme numbers
;-------------------------------------------------------------------------------
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  ;; Ex 2.79:
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  ;; Ex 2.80:
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  ;; Ex 2.81 (given):
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y)))) ; using primitive expt
  ;; Ex 2.83:
  (put 'raise '(scheme-number)
       (lambda (x) (make-rational (contents x) 1)))
  'done)

;;; Constructor 
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;------------------------------------------------------------------------------- 
;        Rational numbers
;-------------------------------------------------------------------------------
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  ;; Ex 2.79:
  (put 'equ? '(rational rational)
       (lambda (x y) (and (= (numer x)
                             (numer y))
                          (= (denom x)
                             (denom y)))))
  ;; Ex 2.80:
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0))) ;; denom check already done
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  ;; Include these proecedures for use in Ex. 2.83 external procedures
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  ;; Ex 2.83:
  (put 'raise '(rational)
       (lambda (x) (make-real (/ (make-real (numer x))
                                 (denom x)))))
  'done)

;;; Constructor
(define (make-rational n d)
  ((get 'make 'rational) n d))

;;; Selectors
(define (numer x) (apply-generic 'numer x))
(define (denom x) (apply-generic 'denom x))

;------------------------------------------------------------------------------- 
;        Real numbers (Ex. 2.83 auxiliary code for testing)
;-------------------------------------------------------------------------------
;;; NOTE: same as scheme-number, but inclue "0.0" or "1.0" to make into decimal
(define (install-real-number-package)
  (define (tag x)
    (attach-tag 'real x))    
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y 0.0))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y 0.0))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y 1.0))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y 1.0))))
  ;; Ex 2.79:
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  ;; Ex 2.80:
  (put '=zero? '(real)
       (lambda (x) (= x 0.0)))
  (put 'make 'real
       (lambda (x) (tag (* 1.0 x)))) ; ensure we have a decimal
  ;; Ex 2.81 (given):
  (put 'exp '(real real)
       (lambda (x y) (tag (expt x y)))) ; using primitive expt
  ;; Ex 2.83:
  (put 'raise '(real)
       (lambda (x) (make-complex-from-real-imag (contents x) 0.0)))
  'done)

;;; Constructor 
(define (make-real n)
  ((get 'make 'real) n))

;------------------------------------------------------------------------------- 
;        Complex numbers
;-------------------------------------------------------------------------------
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
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
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  ;; Ex 2.79:
  (put 'equ? '(complex complex)
       (lambda (x y) (and (= (real-part x)
                             (real-part y))
                          (= (imag-part x)
                             (imag-part y)))))
  ;; Ex 2.80:
  (put '=zero? '(complex)
       (lambda (x) (and (= (real-part x) 0)
                        (= (imag-part x) 0))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  ;; Ex 2.78: Alyssa P. Hacker adds these procedures to provide a second layer
  ;; of tags so that we can access the underlying rectangular/polar procedures:
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  ;; Ex 2.83:
  (put 'raise '(complex)
       (lambda (x) (tag x)))
  'done)

;;; Constructors
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;------------------------------------------------------------------------------- 
;        Coercion
;-------------------------------------------------------------------------------
;;; Put procedure in table
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

;------------------------------------------------------------------------------- 
;        Install packages
;-------------------------------------------------------------------------------
(install-scheme-number-package)
(install-rational-package)
(install-real-number-package)
(install-complex-package)
;;==============================================================================
;;==============================================================================
