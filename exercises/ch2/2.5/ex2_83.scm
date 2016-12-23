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

;;; For each type, raise one level in tower
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
  (define mytower '(scheme-number rational real complex))
  (define (try tower)
    (if (null? (cdr tower)) 
      (error "Could not raise type" (type-tag n))
      (let ((current-type (car tower)) 
            (next-types (cdr tower)) 
            (next-type (cadr tower))) 
          (if (equal? (type-tag n) current-type) 
            ((get-coercion current-type next-type) n) 
            (try next-types)))))
  (try mytower))

;;; Test code:
(define n (make-scheme-number 3))
(define n-rat (scheme-number->rational n))
(define n-real (rational->real n-rat))
(define n-comp (real->complex n-real))
(printval n) ; Value: 3
(printval (raise n)) ; Value: (rational 3 . 1)
(printval (raise n-rat)) ; Value: 3.
(printval (raise n-real)) ; Value: (complex rectangular 3. . 0.)
(printval (raise n-comp)) ; Value: (
;;==============================================================================
;;==============================================================================
