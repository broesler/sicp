;;==============================================================================
;;     File: ex3_77.scm
;;  Created: 09/04/2017, 20:23
;;   Author: Bernie Roesler
;;
;;  Description: delayed integral
;;
;;==============================================================================
(load "streams.scm")

;; Modify to expect delayed integrand
(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (if (stream-null? delayed-integrand)
                 the-empty-stream
                 (let ((integrand (force delayed-integrand)))
                   (integral (delay (stream-cdr integrand))
                             (+ (* dt (stream-car integrand))
                                initial-value)
                             dt)))))

;; Given
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y) 1 1e-4) 1e4) ; Value: 2.7181459268252266

;;==============================================================================
;;==============================================================================
