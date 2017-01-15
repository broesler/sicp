;;==============================================================================
;;     File: ex3_16.scm
;;  Created: 01/14/2017, 13:47
;;   Author: Bernie Roesler
;;
;;  Description: Ben Bitdiddle's incorrect count-pairs
;;
;;==============================================================================
(load "ex3_12-13.scm") ; make-cycle

;;; Defined in exercise
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;;; Test code:
(define x (list 'a 'b))

(newline)
(display "count-pairs poorly:")
(define z3 (cons x 'c))
(printval (count-pairs z3)) ; Value: 3

(define z4 (cons x (cdr x)))
(printval (count-pairs z4)) ; Value: 4

(define a (list 'a))
(define aa (cons a a))
(define z7 (cons aa aa))
(printval (count-pairs z7)) ; Value: 7

(define zinf (make-cycle (list 'a 'b 'c)))
; (count-pairs zinf) ;Aborting!: maximum recursion depth exceeded

;------------------------------------------------------------------------------- 
;        Ex 3.17: properly count pairs
;-------------------------------------------------------------------------------
;;; Traverse the structure, maintaining an auxiliary structure to track which
;;; pairs have already been counted. 
;;; Build a list of pointers to each "counted" pair, then return the length of
;;; the list.
(define (count-pairs x)
  (let ((counted '()))
    (define (count s)
      (cond ((or (not (pair? s)) 
                 (memq s counted)) 
             0)
            (else (set! counted (cons s counted))
                  (+ (count (car s))
                     (count (cdr s))
                     1))))
   (count x)))

(newline)
(display "count-pairs properly:")
(printval (count-pairs z3)) ; Value: 3
(printval (count-pairs z4)) ; Value: 3
(printval (count-pairs z7)) ; Value: 3

;------------------------------------------------------------------------------- 
;        Ex 3.18: detecting cycles
;-------------------------------------------------------------------------------
(define (has-cycle? x)
  (let ((seen '()))
    (define (loop s)
      (cond ((not (pair? s)) #f)
            ((memq s seen) #t)
            (else (set! seen (cons s seen))
                  (loop (cdr s)))))
    (loop x)))

(newline)
(display "has-cycle? non-constant space:")
(printval (has-cycle? z3))   ; Value: #f
(printval (has-cycle? z4))   ; Value: #f
(printval (has-cycle? z7))   ; Value: #f
(printval (has-cycle? zinf)) ; Value: #t

(define ll (list 'a 'b 'c 'd))
(set-cdr! (last-pair ll) (cdr ll)) ; loop b c d
(printval (has-cycle? ll)) ; Value: #t

;------------------------------------------------------------------------------- 
;        Ex 3.19: has-cycle? in constant space
;-------------------------------------------------------------------------------
;;; NOTE: this solution is simple, but it only works if the first element is in
;;; the loop. Use the tortoise and hare algorithm instead.
; (define (has-cycle? x)
;   (let ((temp x)) ; pointer to start of list
;     (define (loop s)
;       (cond ((not (pair? s)) #f)
;             ((eq? (cdr s) temp) #t)
;             (else (loop (cdr s)))))
;     (loop x)))

;;; Tortoise and hare algorithm:
;;; Hare jumps forwards 2 elements at a time, tortoise moves 1. Go until hare
;;; hits the end of the list, or they catch up with each other.
(define (has-cycle? x) 
  (define (seen-last-pair? x)
    (or (not (pair? x))
        (not (pair? (cdr x)))))
  (define (chase turtle rabbit)
    (cond ((or (not (pair? turtle))
               (not (pair? rabbit))
               (seen-last-pair? (cdr rabbit)))
           #f)
          ((eq? turtle rabbit) #t)
          (else (chase (cdr turtle) (cddr rabbit)))))
    (chase x (cdr x)))

(newline)
(display "has-cycle? constant space:")
(printval (has-cycle? z3))   ; Value: #f
(printval (has-cycle? z4))   ; Value: #f
(printval (has-cycle? z7))   ; Value: #f
(printval (has-cycle? zinf)) ; Value: #t
(printval (has-cycle? ll))   ; Value: #t

;;==============================================================================
;;==============================================================================
