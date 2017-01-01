;;==============================================================================
;;     File: ex2_83.scm
;;  Created: 12/23/2016, 16:56
;;   Author: Bernie Roesler
;;
;;  Description: raise a type up the tower
;;    scheme-number -> rational -> real -> complex
;;
;;==============================================================================
(load "generic_arithmetic.scm")

;------------------------------------------------------------------------------- 
;        Method 1
;-------------------------------------------------------------------------------
;; scheme-number -> rational == n/1
(define (scheme-number->rational n)
  (make-rational (contents n) 1))

;; rational -> real == n/d
(define (rational->real n)
    (make-real (/ (make-real (numer n))
                  (denom n))))

;; real -> complex == a + 0i
(define (real->complex n)
  (make-complex-from-real-imag (contents n) 0.0))

;;; Put all types in coercion table
(put-coercion 'scheme-number  'rational  scheme-number->rational)
(put-coercion 'rational       'real      rational->real)
(put-coercion 'real           'complex   real->complex)

;;; Generic raise procedure
(define (raise n)
  (let ((type (type-tag n)))
    (cond ((equal? type 'scheme-number)
           ((get-coercion 'scheme-number 'rational) n))
          ((equal? type 'rational)
           ((get-coercion 'rational 'real) n))
          ((equal? type 'real)
           ((get-coercion 'real 'complex) n))
          ((equal? type 'complex)
           n)
          (else (error "No raise procedure exists for this type!" type)))))

;------------------------------------------------------------------------------- 
;        Method 2
;-------------------------------------------------------------------------------
;;; Put above procedures into each respective package. To add a new type, just
;;; define the two procedures to raise to/from that type.
(define (raise n) (apply-generic 'raise n))

;;; Test code:
(define (repeated f n)
  (define (compose f g)
    (lambda (x) (f (g x))))
  (if (= n 1)
    (lambda (x) (f x))
    (repeated (compose f f) (- n 1))))

(define n (make-scheme-number 3))
; (printval n)                       ; Value: 3
; (printval (raise n))               ; Value: (rational 3 . 1)
; (printval ((repeated raise 2) n))  ; Value: 3.
; (printval ((repeated raise 3) n))  ; Value: (complex rectangular 3. . 0.)
; (printval ((repeated raise 4) n))  ; Value: (complex rectangular 3. . 0.)
;;==============================================================================
;;==============================================================================
