;;==============================================================================
;;     File: polynomials.scm
;;  Created: 01/02/2017, 13:25
;;   Author: Bernie Roesler
;;
;;  Description: Polynomial arithmetic system
;;
;;==============================================================================
(load "ex2_85.scm")

;------------------------------------------------------------------------------- 
;        Polynomial package
;-------------------------------------------------------------------------------
(define (install-polynomial-package)
  ;; Internal procedures
  ;; Representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  ;; Procedures variable? and same-variable? from 2.3.2:
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ;; Representation of terms and term lists
  ;; Terms are represented as: (order coeff) for sparse polynomials
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  ;; Procedures to combine polynomials
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

  ;; Procedures used by add-poly
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))

  ;; Procedures used by mul-poly
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))

  ;; Ex 2.88:
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (add-poly p1 (negate-poly p2))
      (error "Polys not in same var -- SUB-POLY"
             (list p1 p2))))

  ;; Procedures used by sub-poly
  (define (negate-poly p)
    (make-poly (variable p) (negate-terms (term-list p))))

  (define (negate-terms L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t1 (first-term L)))
        (adjoin-term (make-term (order t1)
                                (negate (coeff t1)))
                     (negate-terms (rest-terms L))))))

  ;; Ex 2.87:
  (define (p=zero? p) ; distinguish name from generic =zero?
    (define (all-coeffs-zero? terms)
      (if (null? terms) 
        #t
        (and (=zero? (coeff (first-term terms)))
             (all-coeffs-zero? (rest-terms terms)))))
    ; main procedure
    (let ((terms (term-list p)))
      (or (empty-termlist? terms)
          (all-coeffs-zero? terms))))

  ;; Interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p))))
  ;; Ex 2.87:
  (put '=zero? '(polynomial) p=zero?)
  'done)

;;; Constructor
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;;; Run the procedure
(install-polynomial-package)
;;==============================================================================
;;==============================================================================
