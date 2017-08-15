;;==============================================================================
;;     File: ex3_50.scm
;;  Created: 08/13/2017, 21:41
;;   Author: Bernie Roesler
;;
;;  Description: general stream-map
;;
;;==============================================================================
(load "streams.scm")

;;; Footnote 12, 2.2.3: 
;;;   Scheme standardly provides a map procedure that is more general than the
;;;   one described here. This more general map takes a procedure of
;;;   n arguments, together with n lists, and applies the procedure to all the
;;;   first elements of the lists, all the second elements of the lists, and so
;;;   on, returning a list of the results.

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;;; Convert list to stream for ease of testing
(define (list-stream lst)
  (if (null? lst)
    the-empty-stream
    (cons-stream (car lst)
                 (list-stream (cdr lst)))))

;;; Test code:
(define x1 (list-stream (list   1   2   3)))
(define x2 (list-stream (list  40  50  60)))
(define x3 (list-stream (list 700 800 900)))

(display-stream (stream-map + x1 x2 x3))
; Value: 741 852 963 (as a stream, not a list!)

(display-stream (stream-map (lambda (x y) (+ x (* 2 y)))
                            (list-stream (list 1 2 3))
                            (list-stream (list 4 5 6))))
; Value: 9 12 15

;;; These also work!
; (printval (map + (list 1 2 3) (list 40 50 60) (list 700 800 900)))
; Value: (741 852 963)

; (printval (map (lambda (x y) (+ x (* 2 y)))
;                (list 1 2 3)
;                (list 4 5 6)))
; Value: (9 12 15)


;;==============================================================================
;;==============================================================================
