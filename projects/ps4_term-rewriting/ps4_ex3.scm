;;==============================================================================
;;     File: ps4_ex3.scm
;;  Created: 11/23/2016, 13:02
;;   Author: Bernie Roesler
;;
;;  Description: Desugaring additional expressions
;;
;;==============================================================================
(load "ps4go.scm")

;------------------------------------------------------------------------------- 
;        3.A Add case let* to desugar
;-------------------------------------------------------------------------------
;;; (let* ((<var1> <init1>) 
;;;         ... 
;;;        (<varn> <initn>))
;;;   <body>)
;;; ==>
;;; (let ((<var1> <init1>))
;;;   (let* ((<var2> <init2>)
;;;          ...
;;;          (<varn> <initn>)) 
;;;     <body>))

;;; DESUGAR: <body in extended grammar> --> <body in kernel grammar>
(define (desugar body)
  (cond ((cond? body)
         (let ((clauses (clauses-of-cond body)))
           (if (null? clauses)
               submodel-useless-value
               (let ((first-clause (car clauses)))
                 (desugar
                  (if (else-clause? first-clause)
                      (expression-of-clause first-clause)
                      (make-if
                       (test-of-clause first-clause)
                       (expression-of-clause first-clause)
                       (make-cond (cdr clauses)))))))))
        ((let? body)
         (desugar
          (let ((bindings (bindings-of-let body)))
            (make-combination
             (make-lambda
              (map variable-of-binding bindings)
              (body-of-let body))
             (map init-of-binding bindings)))))
        ;;;;; New let* here: ;;;;;
        ((let*? body) 
         (desugar
           (let ((bindings (bindings-of-let* body)))
             (if (null? bindings)
               (body-of-let* body) ; no assignments so just return the body
               (make-combination   ; skip the "let" statement
                 (make-lambda
                   (list (variable-of-binding (car bindings))) 
                   (make-let* (cdr bindings) (body-of-let* body)))
                 (list (init-of-binding (car bindings))))))))
        ;;;;;;;;;;;;;;;;;;;;;;;;;
        ((or (self-evaluating? body)
             (symbol-expression? body)
             (variable? body)
             (submodel-null? body))
         body)
        ((lambda-expression? body)
         (make-lambda
          (formals-of-lambda body)
          (desugar (body-of-lambda body))))
        ((combination? body)
         (make-combination
          (desugar (operator-of body))
          (map desugar (operands-of body))))
        ((if? body)
         (make-if
          (desugar (test-of-if body))
          (desugar (consequent-of body))
          (desugar (alternative-of body))))
        (else
         (let ((defs (defines-of-body body)))
           (make-body
            (map make-define
                 (map variable-of-define defs)
                 (map desugar (map expression-of-define defs)))
            (desugar (expression-of-body body)))))))

;;; Helper functions
;;; Predicate:
(define let*? (tagged-pair? 'let*))
(define bindings-of-let* bindings-of-let)
(define body-of-let* body-of-let)

;;; Constructors
(define (make-let* bindings body) 
  (list 'let* bindings body))

;------------------------------------------------------------------------------- 
;        3.B Test code:
;-------------------------------------------------------------------------------
(define let*-mytest
  '(let* ((a (+ 2 3))
          (b (inc a)))
     (* b a)))

;;; Should return:
; (let ((a (+ 2 3)))
;   (let* ((b (inc a)))
;     (* b a)))
;; then:
; ((lambda (a) 
;    (let ((b (inc a)))
;    (* b a))) 
;  (+ 2 3))
;; finally:
; ((lambda (a)
;    ((lambda (b) (* b a))
;     (inc a)))
;  (+ 2 3))

(newline)
(display ";;;;;;;;;; mytest ;;;;;;;;;;")
(newline)
(pp (desugar let*-mytest))
;;; Output:
  ; ((lambda (a) 
  ;    ((lambda (b) (* b a)) 
  ;     (inc a))) 
  ;  (+ 2 3))

(define let*-test1
  '((define (put-in-standard-position curve)
      (let* ((start-point (curve 0))
             (curve-started-at-origin
               ((translate (- (x-of start-point))
                           (- (y-of start-point)))
                curve))
             (new-end-point (curve-started-at-origin 1))
             (theta (atan (y-of new-end-point) (x-of new-end-point)))
             (curve-ended-at-x-axis
               ((rotate-around-origin (- theta)) curve-started-at-origin))
             (end-point-on-x-axis (x-of (curve-ended-at-x-axis 1))))
        ((scale (/ 1 end-point-on-x-axis)) curve-ended-at-x-axis)))
    (put-in-standard-position (compose unit-circle double))))

(newline)
(display ";;;;;;;;;; test1 ;;;;;;;;;;")
(newline)
(pp (desugar let*-test1))
;;; Output:
  ; ((define put-in-standard-position
  ;    (lambda (curve)
  ;      ((lambda (start-point)
  ;         ((lambda (curve-started-at-origin)
  ;            ((lambda (new-end-point)
  ;               ((lambda (theta)
  ;                  ((lambda (curve-ended-at-x-axis)
  ;                     ((lambda (end-point-on-x-axis)
  ;                        ((scale (/ 1 end-point-on-x-axis)) curve-ended-at-x-axis))
  ;                      (x-of (curve-ended-at-x-axis 1))))
  ;                   ((rotate-around-origin (- theta)) curve-started-at-origin)))
  ;                (atan (y-of new-end-point) (x-of new-end-point))))
  ;             (curve-started-at-origin 1)))
  ;          ((translate (- (x-of start-point)) (- (y-of start-point))) curve)))
  ;       (curve 0))))
  ;  (put-in-standard-position (compose unit-circle double)))

(newline)
(display ";;;;;;;;;; test2 ;;;;;;;;;;")
(newline)
(define let*-test2
  '(let* ((a (+ 2 3))
          (b (inc a))
          (b ((lambda ()
                (define (y) (let* ((a (inc a))
                                   (b (inc a)))
                              (cons b y)))
                y))))
     (not (let* ((c (cdr (b))))
            (equal? c b)))))

(pp (desugar let*-test2))

;;; Output:
  ; ((lambda (a)
  ;    ((lambda (b)
  ;       ((lambda (b)
  ;          (not ((lambda (c) (equal? c b)) (cdr (b)))))
  ;        ((lambda ()
  ;           (define y
  ;             (lambda ()
  ;               ((lambda (a) ((lambda (b) (cons b y)) (inc a))) (inc a))))
  ;           y))))
  ;     (inc a)))
  ;  (+ 2 3))

;;==============================================================================
;;==============================================================================
