;;==============================================================================
;;     File: streams.scm
;;  Created: 06/19/2017, 18:23
;;   Author: Bernie Roesler
;;
;;  Description: Stream toy code
;;
;;==============================================================================
(load "../1.2/ex1_22-24.scm") ; Fermat prime? test
; (load "../2.2/sequence_operations.scm") ; enumerate-interval

;------------------------------------------------------------------------------- 
;        Basic stream operations
;-------------------------------------------------------------------------------
; (car (cdr (filter prime?
;                   (enumerate-interval 10000 1000000))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

;; Display n elements of stream
(define (display-stream-n s n)
  (define irange (enumerate-interval 0 (- n 1)))
  (define (p-val x) (printval (stream-ref s x)))
  (for-each p-val irange)
  (newline))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;;; Test code:
(define (prime? n)
  (fast-prime? n 100))

;; Original code:
; (stream-car
;  (stream-cdr
;   (stream-filter prime?
;                  (stream-enumerate-interval 10000 1000000))))

;; Define stream-cadr
(define (stream-cadr x) (stream-car (stream-cdr x)))

;;; Test code:
; (stream-cadr
;   (stream-filter prime?
;                  (stream-enumerate-interval 10000 1000000)))

;;; From SICP support code:
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

;;; Ex 3.50 (see separate file)
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))

;; EXERCISE 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

;; EXERCISE 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))


;;; Useful: Enumerate items in a sequence
(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

;;; Convert list to stream for ease of testing
(define (list-stream lst)
  (if (null? lst)
    the-empty-stream
    (cons-stream (car lst)
                 (list-stream (cdr lst)))))

;;==============================================================================
;;==============================================================================
