;;==============================================================================
;;     File: ex2_29.scm
;;  Created: 11/11/2016, 14:35
;;   Author: Bernie Roesler
;;
;;  Description: binary mobile
;;
;;==============================================================================

;; mobile constructor: takes two lists (branch or mobile)
(define (make-mobile left right)
  (list left right))

;; branch constructor: takes number and (number or list)
;;  struct is either a number == weight, or another mobile
(define (make-branch len struct)
  (list len struct))

;------------------------------------------------------------------------------- 
;       (a) write selectors 
;-------------------------------------------------------------------------------
;; Mobile selectors
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

;; Branch selectors
(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

;------------------------------------------------------------------------------- 
;       (b) total-weight (sum of all lengths)
;-------------------------------------------------------------------------------
;; like count-leaves, but instead of adding one, add the weight of the branch
(define (total-weight x)
  (cond ((null? x) 0)
        ((not (pair? x)) x)
        (else (+ (total-weight (branch-structure (left-branch x)))
                 (total-weight (branch-structure (right-branch x)))))))

;------------------------------------------------------------------------------- 
;       (c) balanced? 
;-------------------------------------------------------------------------------
;;; predicate to determine whether torque applied by each branch is equal for
;;; current mobile and for all sub-mobiles
(define (balanced? x)
  (cond ((null? x) #f)
        ((not (pair? x)) #t)
        (else 
          (let ((la (branch-length (left-branch x)))
                (lb (branch-length (right-branch x)))
                (wa (total-weight (branch-structure (left-branch x))))
                (wb (total-weight (branch-structure (right-branch x)))))
          (and 
            ;; if current mobile is balanced
            (= (* la wa ) 
               (* lb wb ))
            ;; and if each sub-branch is balanced
            (balanced? (branch-structure (left-branch x)))
            (balanced? (branch-structure (right-branch x))))))))

;;; Make simple two-branch mobile:
;;            m1
;;             |
;;         b1 /\ b2
;;           2  \
;;               1
(define b1 (make-branch 1 2))
(define b2 (make-branch 2 1))
(define m1 (make-mobile b1 b2))

;;; Test procedures:
(newline)
(display "Test small mobile: ")
(printval m1) ; Value: ((1 2) (2 1))
(printval (left-branch m1))       ; Value: (1 2)
(printval (right-branch m1))      ; Value: (2 1)
(printval (branch-length b1))     ; Value: 1
(printval (branch-structure b1))  ; Value: 2
(printval (total-weight m1))      ; Value: 3
(printval (balanced? m1))         ; Value: #t

;;; Make complate multi-branch mobile:
;;            mm
;;             |
;;         b1 /\ b6
;;           6  \  <-m2
;;           b5 /\ b4
;;       m1->  /  \
;;         b2 /\b3 3
;;            2 \
;;               1
;; 
;;; Branches with "leaves"
(define b1 (make-branch 1 8)) ; change to 7 for (balanced?) == #f, 6 for #t
(define b2 (make-branch 1 2))
(define b3 (make-branch 2 1))
(define b4 (make-branch 1 3))
;;; Branches of mobiles
(define m1 (make-mobile b2 b3))
(define b5 (make-branch 1 m1))
(define m2 (make-mobile b5 b4))
(define b6 (make-branch 1 m2))
(define mm (make-mobile b1 b6))  ; top-level mobile

;;; Test procedures:
(newline)
(display "Test large mobile: ")
(printval mm) ; Value: ((1 6) (1 ((1 ((1 2) (2 1))) (1 3)))) 
(printval (total-weight mm)) ; Value: 14
(printval (balanced? mm))    ; Value: #f

;;==============================================================================
;;==============================================================================
