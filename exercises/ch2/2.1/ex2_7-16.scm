;;==============================================================================
;;     File: ex2_7-16.scm
;;  Created: 11/09/2016, 19:24
;;   Author: Bernie Roesler
;;
;;  Description: Extended example: interval arithmetic
;;
;;==============================================================================

;;; An interval is, say "6.5 ± 0.5" => "(6.0, 7.0)"

;; Basic interval operations (given)
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;------------------------------------------------------------------------------- 
;        Ex 2.7 make the constructor and selectors
;-------------------------------------------------------------------------------
(define (make-interval a b) (cons a b))
(define (lower-bound x) (min (car x) (cdr x)))
(define (upper-bound x) (max (car x) (cdr x)))

;; Test code:
(define x (make-interval 1.0 2.0))
(define y (make-interval 4.0 3.0))
(newline)
(display (lower-bound x))
(newline)
(display (upper-bound x))
(newline)
(display (lower-bound y))
(newline)
(display (upper-bound y))

;------------------------------------------------------------------------------- 
;        Ex 2.8 subtract intervals
;-------------------------------------------------------------------------------
; min == smallest of x - largest of y
; max == largest of x - smallest of y
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(newline)
(display (add-interval x y))
(newline)
(display (sub-interval x y))
(newline)
(display (mul-interval x y))
(newline)
(display (div-interval x y))

;------------------------------------------------------------------------------- 
;        Ex 2.9 width 
;-------------------------------------------------------------------------------
(define (interval-width x)
  (/ (- (upper-bound x) (lower-bound x)) 
     2))

(newline)
(display (interval-width x))

;------------------------------------------------------------------------------- 
;        Ex 2.10 divide by 0?
;-------------------------------------------------------------------------------
; Redefine div-interval
(define (div-interval x y)
  (if (= (lower-bound y) (upper-bound y))
    (error "Divide by 0! y = " y)
    (mul-interval x 
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))
(newline)
(display (div-interval x y))
; These lines produce error!
; (define y (make-interval 7 7))
; (newline)
; (display (div-interval x y))

;------------------------------------------------------------------------------- 
;        Ex 2.11
;-------------------------------------------------------------------------------
; redefine mul-interval to check signs, limiting multiplications
(define (mul-interval x y)
  (let ((lbx (lower-bound x))
        (ubx (upper-bound x))
        (lby (lower-bound y))
        (uby (upper-bound y)))
    (cond ((and (and (<  lbx 0) (<  ubx 0)) (and (<  lby 0) (<  uby 0))) ; 1 - - - -
           (make-interval (* lbx lby) (* ubx uby)))
          ((and (and (<  lbx 0) (>= ubx 0)) (and (<  lby 0) (<  uby 0))) ; 2 - + - -
           (make-interval (* ubx lby) (* lbx lby)))
          ((and (and (<  lbx 0) (>= ubx 0)) (and (<  lby 0) (>= uby 0))) ; 3 - + - +
           (make-interval (min (* lbx uby) (* ubx lby))
                          (max (* lbx lby) (* ubx uby))))
          ((and (and (<  lbx 0) (>= ubx 0)) (and (>= lby 0) (>= uby 0))) ; 4 - + + +
           (make-interval (* lbx uby) (* ubx uby)))
          ((and (and (>= lbx 0) (>= ubx 0)) (and (>= lby 0) (>= uby 0))) ; 5 + + + +
           (make-interval (* lbx lby) (* ubx uby)))
          ((and (and (>= lbx 0) (>= ubx 0)) (and (<  lby 0) (>= uby 0))) ; 6 + + - +
           (make-interval (* ubx lby) (* lbx uby)))
          ((and (and (>= lbx 0) (>= ubx 0)) (and (<  lby 0) (<  uby 0))) ; 7 + + - -
           (make-interval (* ubx lby) (* lbx uby)))
          ((and (and (<  lbx 0) (<  ubx 0)) (and (<  lby 0) (>= uby 0))) ; 8 - - - +
           (make-interval (* lbx uby) (* lbx lby)))
          ((and (and (<  lbx 0) (<  ubx 0)) (and (>= lby 0) (>= uby 0))) ; 9 - - + +
           (make-interval (* lbx uby) (* ubx lby))))))

;; Test code:
(newline)
(display (mul-interval x y))

;------------------------------------------------------------------------------- 
;        Ex 2.12
;-------------------------------------------------------------------------------
; Now make the number as "6.5 ± 0.5"
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; Unfortunately, need to use center and percentage, so define a constructor
(define (make-center-percent c p)
  (let ((dc (* c p)))
    (make-interval (- c dc) (+ c dc))))

;; percentage selector (center + width defined above)
(define (percent i)
  (let ((l (lower-bound i))
        (u (upper-bound i)))
    (/ (- u l) (+ u l)))) ; i.e. w/c * 100

;; Test code:
(define x (make-center-percent 6.0 5))
(newline)
(display (center x))
(newline)
(display (percent x))
;;==============================================================================
;;==============================================================================
