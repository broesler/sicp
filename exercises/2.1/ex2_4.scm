;;==============================================================================
;;     File: ex2_4.scm
;;  Created: 11/09/2016, 00:03
;;   Author: Bernie Roesler
;;
;;  Description: Alternative representations
;;
;;==============================================================================

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

; corresponding cdr
(define (cdr z)
  (z (lambda (p q) q)))

;;; Verify that (car (cons x y)) == x
;;; substitute:
; (car (cons x y))
; (car (lambda (m) (m x y)))
; ((lambda (m) (m x y)) (lambda (p q) p))
; ((lambda (p q) p) x y)
; x

;; Test code:
(newline)
(display (car (cons 1 2)))
(newline)
(display (cdr (cons 1 2)))
;;==============================================================================
;;==============================================================================
