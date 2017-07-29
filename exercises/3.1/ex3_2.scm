;;==============================================================================
;;     File: ex3_2.scm
;;  Created: 01/11/2017, 10:39
;;   Author: Bernie Roesler
;;
;;  Description: monitoring procedures calls
;;
;;==============================================================================

;;; make-monitored : f(x) --> 
(define (make-monitored f)
  (define calls 0)
  (define (mf m)
    (cond ((eq? m 'how-many-calls?) 
           calls)
          ((eq? m 'reset-count)
           (set! calls 0) 
           0)
          (else 
            (set! calls (1+ calls)) 
            (f m))))
  mf)

;;; Test code:
(define s (make-monitored sqrt))
(printval (s 100))                 ; Value: 10
(printval (s 169))                 ; Value: 13
(printval (s 'how-many-calls?))    ; Value: 2
(printval (s 'reset-count))        ; Value: 0
(printval (s 2))                   ; Value: 1.4142135623730951
(printval (s 'how-many-calls?))    ; Value: 1

;;==============================================================================
;;==============================================================================
