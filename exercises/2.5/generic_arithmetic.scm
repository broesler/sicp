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
(define (raise n) (apply-generic 'raise n))   ; Ex 2.83

;;; Ex 2.86:
(define (square-root x) (apply-generic 'square-root x))
(define (square x) (apply-generic 'square x))
(define (sine x) (apply-generic 'sine x))
(define (cose x) (apply-generic 'cose x))
(define (arctan x y) (apply-generic 'arctan x y))

;;; Ex 2.88:
(define (negate x) (apply-generic 'negate x))

;;; Ex 2.94:
(define (greatest-common-divisor a b) (apply-generic 'gcd a b))

;;; Ex 2.97:
(define (reduce n d) (apply-generic 'reduce n d))

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
  ;; internal procedures
  (define (tag x)
    (attach-tag 'scheme-number x))
  ;; Ex 2.97: reduce-integers : (RepNum, RepNum) --> (RepNum, RepNum)
  (define (reduce-integers n d)
    (let ((g (gcd n d)))
      (list (/ n g) (/ d g))))
  ;; interface with system
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
  ;; Ex 2.85:
  (put 'project '(scheme-number)
       (lambda (x) (make-scheme-number x)))
  ;; Ex 2.86:
  (put 'square-root '(scheme-number)
       (lambda (x)
         (let ((root (sqrt x)))
           (make-complex-from-real-imag (make-real (real-part root))
                                        (make-real (imag-part root))))))
  (put 'square '(scheme-number)
       (lambda (x) (tag (* x x))))
  (put 'sine '(scheme-number)
       (lambda (x) (make-real (sin x))))
  (put 'cose '(scheme-number)
       (lambda (x) (make-real (cos x))))
  (put 'arctan '(scheme-number scheme-number)
       (lambda (x y) (make-real (atan x y))))
  ;; Ex 2.88:
  (put 'negate '(scheme-number)
       (lambda (x) (tag (- x))))
  ;; Ex 2.94:
  (put 'gcd '(scheme-number scheme-number) gcd)
  ;; Ex 2.97:
  (put 'reduce '(scheme-number scheme-number) reduce-integers)
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
    (let ((nd (reduce n d))) ; Ex 2.93,97 use generic reduce
      (cons (car nd)         ; convert list to single pair
            (cadr nd))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; Ex 2.86:
  (define (sqrt-rat x)
    (let ((root (sqrt (/ (numer x) (denom x)))))
      (make-complex-from-real-imag (make-real (real-part root))
                                   (make-real (imag-part root)))))
  (define (square-rat x) (mul-rat x x))
  (define (sin-rat x)
    (sin (/ (numer x)
            (denom x))))
  (define (cos-rat x)
    (cos (/ (numer x)
            (denom x))))
  (define (atan-rat x y)
    (atan (/ (numer x) (denom x))
          (/ (numer y) (denom y))))
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
       (lambda (x) (/ (make-real (numer x))
                      (denom x))))
  ;; Ex 2.85:
  (put 'project '(rational)
       (lambda (x) (make-scheme-number (inexact->exact (round (/ (numer x)
                                                                 (denom x)))))))
  ;; Ex 2.86:
  (put 'square-root '(rational)
       (lambda (x) (make-real (sqrt-rat x))))
  (put 'square '(rational)
       (lambda (x) (tag (square-rat x))))
  (put 'sine '(rational)
       (lambda (x) (make-real (sin-rat x))))
  (put 'cose '(rational)
       (lambda (x) (make-real (cos-rat x))))
  (put 'arctan '(rational rational)
       (lambda (x) (make-real (atan-rat x y))))
  ;; Ex 2.88:
  (put 'negate '(rational)
       (lambda (x) (make-rat (negate (numer x))
                             (denom x))))
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
       (lambda (x) (tag (exact->inexact x)))) ; ensure we have a decimal
  ;; Ex 2.81 (given):
  (put 'exp '(real real)
       (lambda (x y) (tag (expt x y)))) ; using primitive expt
  ;; Ex 2.83:
  (put 'raise '(real)
       (lambda (x) (make-complex-from-real-imag x 0.0)))
  ;; Ex 2.85:
  (put 'project '(real)
       (lambda (x) (make-rational (round x) 1.0)))
       ; (lambda (x) (make-rational (inexact->exact (numerator x))
       ;                            (inexact->exact (denominator x)))))
       ; (lambda (x) (make-rational (numerator (rationalize (inexact->exact x)
       ;                                                    1/100))
       ;                            (denominator (rationalize (inexact->exact x)
       ;                                                      1/100)))))
  ;; Ex 2.86:
  (put 'square-root '(real)
       (lambda (x)
         (let ((root (sqrt x)))
           (make-complex-from-real-imag (tag (real-part root))
                                        (tag (imag-part root))))))
  (put 'square '(real)
       (lambda (x) (tag (* x x 1.0))))
  (put 'sine '(real)
       (lambda (x) (tag (+ (sin x) 0.0))))
  (put 'cose '(real)
       (lambda (x) (tag (+ (cos x) 0.0))))
  (put 'arctan '(real)
       (lambda (x) (tag (+ (atan x) 0.0))))
  ;; Ex 2.88:
  (put 'negate '(real)
       (lambda (x) (tag (- x))))
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
    (make-from-real-imag (add (c-real-part z1) (c-real-part z2))
                         (add (c-imag-part z1) (c-imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (c-real-part z1) (c-real-part z2))
                         (sub (c-imag-part z1) (c-imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (c-magnitude z1) (c-magnitude z2))
                       (add (c-angle z1) (c-angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (c-magnitude z1) (c-magnitude z2))
                       (sub (c-angle z1) (c-angle z2))))
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
       (lambda (x y) (and (equ? (c-real-part x) (c-real-part y))
                          (equ? (c-imag-part x) (c-imag-part y)))))
  ;; Ex 2.80:
  (put '=zero? '(complex)
       (lambda (x) (and (=zero? (c-real-part x))
                        (=zero? (c-imag-part x)))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  ;; Ex 2.78: Alyssa P. Hacker adds these procedures to provide a second layer
  ;; of tags so that we can access the underlying rectangular/polar procedures:
  (put 'c-real-part '(complex) c-real-part)
  (put 'c-imag-part '(complex) c-imag-part)
  (put 'c-magnitude '(complex) c-magnitude)
  (put 'c-angle '(complex) c-angle)
  ;; Ex 2.83:
  (put 'raise '(complex)
       (lambda (x) (tag x)))
  ;; Ex 2.85:
  (put 'project '(complex)
       (lambda (x) (project (c-real-part x)))) ; Ex 2.86
       ; (lambda (x) (make-real (c-real-part x)))) ; prior to 3x 2.86
  ;; Ex 2.88:
  (put 'negate '(complex)
       (lambda (x) (make-complex-from-real-imag (negate (c-real-part x))
                                                (negate (c-imag-part x)))))
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
