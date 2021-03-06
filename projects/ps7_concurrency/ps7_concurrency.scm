;;==============================================================================
;;     File: ps7_concurrency.scm
;;  Created: 06/19/2017, 21:11
;;   Author: Bernie Roesler
;;
;;  Description: Responses to the exercises in sample problem set: 
;;      Simulation and Concurrency: How England Lost Her Barings
;;
;;==============================================================================
(define load-ps 
  (lambda ()
    (load "gauss.scm")
    (load "parallel.scm")
    (load "nikkei.scm")))

;------------------------------------------------------------------------------- 
;       Exercises 
;-------------------------------------------------------------------------------
; (newline) (display ";;;;;;;;;; Exercise 1 ;;;;;;;;;;")
;
; In general, we need serializers to implement a market because multiple actors
; all need to access the same "account" (i.e. the prices). We need two different
; serializers to implement a market to separate the processing of price changes
; with the processing of orders. 
;
; If we only used one serialized and serialized all three of the guarded
; regions, then prices could be manipulated while orders were being processed,
; and mayhem ensues.
;
; The orders-serializer *must* be used for guarding two regions (new-order! and
; execute-and-order) because they both access and manipulate the pending-orders
; list.

; (newline) (display ";;;;;;;;;; Exercise 2 ;;;;;;;;;;")
;
;;; Correct code (nikkei.scm):
((eq? m 'execute-an-order)
 (((orders-serializer
     (lambda ()
       (if (not (null? pending-orders))
         (let ((outstanding-order (car pending-orders)))
           (set! pending-orders (cdr pending-orders))
           outstanding-order)
         (lambda () 'nothing-to-do)))))))

;;; Louis Reasoner's suggestion:
((eq? m 'execute-an-order)
 ((orders-serializer
    (lambda ()
      (if (not (null? pending-orders))
        (begin ((car pending-orders))
               (set! pending-orders (cdr pending-orders))))))))

;;; Slightly better idea:
((eq? m 'execute-an-order)
 (let ((current-order (lambda () 'nothing-to-do)))
   (if (not (null? pending-orders))
     ((orders-serializer
        (lambda ()
          (begin (set! current-order (car pending-orders))
                 (set! pending-orders (cdr pending-orders)))))))
   (current-order)))

; Louis' idea will fail if the market wishes to execute two orders at the same
; time. We must move the pointer to the head of the pending-orders list BEFORE
; we execute the first order, in effect protecting the first order from being
; executed twice. The (let ((outstanding-order...))) statement handles this
; protection.
;
; The second idea is better because it achieves the protection of the first
; order, but removes the execution of the order from the orders-serializer.
; Thus, the pending-orders list may be altered by a new-order! before it is
; executed.

; (newline) (display ";;;;;;;;;; Exercise 3 ;;;;;;;;;;")
; The shared values protected by make-arbitrager are:
;   1. balance
;   2. contracts
; trader-serializer serializes the set! operation on balance and contracts, such
; that the traders' account always has a balance and number of contracts that
; are self-consistent.

; (newline) (display ";;;;;;;;;; Exercise 4 ;;;;;;;;;;")
; Leeson's anomalous contracts values appear when the audit happens between
; arbitrage trades. Leeson holds a position only for the time it takes to
; complete two transactions, so, occasionally, the audit occurs between these
; transactions. We could set up the simulation such that audits only occur at
; the end of the "day", or at set times, and only have Nick complete his
; arbitrage trades between those times. 

; (newline) (display ";;;;;;;;;; Exercise 5 ;;;;;;;;;;")
;;==============================================================================
;;==============================================================================
