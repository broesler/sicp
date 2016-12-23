;;==============================================================================
;;     File: ex2_83.scm
;;  Created: 12/23/2016, 16:56
;;   Author: Bernie Roesler
;;
;;  Description: raise a type up the tower
;;    integer -> rational -> real -> complex
;;
;;==============================================================================

;;; For each type, raise one level in tower
;; integer -> rational == n/1
(define (scheme-number->rational n)
  (make-rational (contents n) 1))

;; rational -> real == n/d
(define (rational->real n)
  (define (integer->float x)
    (* x 1.0))
  (let (v (contents n))
    (make-real (/ (integer->float (numer v))
                  (denom v)))))

;; real -> complex == a + 0i
(define (real->complex n)
  (make-complex-from-real-imag (contents n) 0))

;;; Put all types in coercion table
(put-coercion 'scheme-number 'rational  scheme-number->rational)
(put-coercion 'rational      'real      rational->real)
(put-coercion 'real          'complex   real->complex)

;;; Generic raise procedure
(define (raise n)
  (let (type (type-tag n))
    (cond ((equal? type-tag 'scheme-number)
           ((get-coercion 'scheme-number 'rational) n))
          ((equal? type-tag 'rational)
           ((get-coercion 'rational 'real) n))
          ((equal? type-tag 'real)
           ((get-coercion 'real 'complex) n))
          ((equal? type-tag 'complex)
           n)
          (else (error "No raise procedure exists for this type!" type)))))


;;==============================================================================
;;==============================================================================
