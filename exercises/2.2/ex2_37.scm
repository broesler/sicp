;;==============================================================================
;;     File: ex2_37.scm
;;  Created: 11/13/2016, 17:15
;;   Author: Bernie Roesler
;;
;;  Description: Matrix operations with sequences
;;
;;==============================================================================
(load "ex2_36.scm") ; for (accumulate-n) (also loads "sequence_operations.scm")

;;; Define the matrix
;;    [ 1 2 3 4
;;      4 5 6 6
;;      6 7 8 9 ]
;; as the list
;;    ((1 2 3 4) (4 5 6 6) (6 7 8 9))
;;
;;; Mathematical definitions:
;;    (dot-product v w) = scalar sum_i v_i*w_i
;;    (matrix-*-vector M v) = vector t, where t_i = sum_j m_ij*v_j
;;    (matrix-*-matrix M N) = matrix P, where p_ij = sum_k m_ik*n_kj
;;    (transpose M) = matrix N, where n_ij = m_ji

;;; Procedure definitions:
;; NOTE: Need to use raw environment to get built-in (map) definition
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define (transpose m)
  (accumulate-n cons '() m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

;;; Test code:
(define v (list 1 2 3 4))
(define w (list 5 6 7 8))
(define M (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(printval (dot-product v w))      ; Value: 70
(printval (matrix-*-vector M v))  ; Value: (30 56 80)
(printval (transpose M))          ; Value: ((1 4 6) (2 5 7) (3 6 8) (4 6 9))
(printval (matrix-*-matrix (transpose M) M)) 
; Value: ((53  64  75  82) 
;         (64  78  92 101) 
;         (75  92 109 120) 
;         (82 101 120 133))
;;==============================================================================
;;==============================================================================
