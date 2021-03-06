;;==============================================================================
;;     File: polynomials.scm
;;  Created: 01/02/2017, 13:25
;;   Author: Bernie Roesler
;;
;;  Description: Polynomial arithmetic system
;;
;;==============================================================================
(load "ex2_84.scm")

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

  ;; Procedures used by add-poly (union-set for ordered sets)
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
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

  (define (sub-terms L1 L2)
    (add-terms L1 (negate-terms L2)))

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

  ;; Equality
  ;; (RepPoly, RepPoly) --> Bool
  (define (equ-poly? p1 p2)
    (=zero-poly? (sub-poly p1 p2)))

  ;; Ex 2.91: Division
  ;; (RepPoly, RepPoly) --> ({polynomial} X RepPoly, {polynomial} X RepPoly)
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (let ((qr (div-terms (term-list p1)
                           (term-list p2))))
        ;; return both quotient and remainder
        (list (make-polynomial (variable p1)
                               (car qr))
              (make-polynomial (variable p1)
                               (cadr qr))))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))

  ;; Procedures used by div-poly
  ;; (RepTerms, RepTerms) --> (RepPoly, RepPoly)
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
          (list (the-empty-termlist) L1)
          (let ((new-c (div (coeff t1) (coeff t2)))
                (new-o (- (order t1) (order t2))))
            (let* ((quo (make-term new-o new-c))
                   (rest-of-result
                     (div-terms (sub-terms L1 (mul-terms (list quo) L2)) L2)))
              ; return (quotient remainder)
              (list (adjoin-term quo (car rest-of-result))
                    (cadr rest-of-result))))))))

  ;; Ex 2.94:
  ;; (RepPoly, RepPoly) --> RepPoly
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1) (gcd-terms (term-list p1)
                                          (term-list p2)))
      (error "Polys not in same var -- GCD-POLY"
             (list p1 p2))))

  ;; (RepTerms, RepTerms) --> RepTerms
  (define (gcd-terms a b)
    (if (empty-termlist? b)
      (reduce-coeffs a) ; Ex 2.96(b)
      (gcd-terms b (pseudoremainder-terms a b))))

  ;; procedures used by gcd-terms
  ;; Ex 2.94: plain remainder
  ;; (RepTerms, RepTerms) --> RepTerms
  ; (define (remainder-terms a b)
  ;   (cadr (div-terms a b)))

  ;; Ex 2.96(a): replace above with pseudoremainder
  ;; (RepTerms, RepTerms) --> RepTerms
  (define (pseudoremainder-terms a b)
    (let ((integerize (make-integerizer a b)))
      (cadr (div-terms (integerize a) b))))

  ;; integerize p, using q to determine integerization factor
  ;; (RepTerms, RepTerms) -> (RepTerms --> RepTerms)
  (define (make-integerizer p q)
    (lambda (pp)
      (apply-const mul-terms pp (integerize-factor p q))))

  ;; integerize-factor : (RepTerms, RepTerms) --> Sch-NatNum
  (define (integerize-factor p q)
    (if (or (empty-termlist? p) 
            (empty-termlist? q))
      1
      (let ((p1 (first-term p))
            (q1 (first-term q)))
        (let ((o1 (order p1))
              (o2 (order q1))
              (c  (coeff q1)))
          (expt c (+ 1 o1 (- o2)))))))

  ;; Ex 2.96(b)
  ;; Reduce poly coefficients to lowest terms by their gcd
  ;; ASSUMES COEFFS ARE ALL INTEGERS!!!
  ;; If first term coeff is < 0, flip the signs, so we get the "absolute value"
  ;; of the polynomial gcd
  ;; RepTerms -> RepTerms
  (define (reduce-coeffs p)
    (if (< (coeff (first-term p)) 0)
      (car (apply-const div-terms (negate-terms p) (gcd-coeffs p)))
      (car (apply-const div-terms               p  (gcd-coeffs p)))))

  ;; Get gcd of all coefficients 
  ;; ASSUMES COEFFS ARE ALL INTEGERS!!!
  ;; RepTerms -> Sch-Num
  (define (gcd-coeffs p)
    (if (empty-termlist? p)
      0
      (greatest-common-divisor (coeff (first-term p))
                               (gcd-coeffs (rest-terms p)))))

  ;; Ex 2.97 (a): Reduce 2 polynomials' terms to lowest factors via their gcd
  ;; (ASSUMES COEFFS ARE ALL INTEGERS!!!)
  ;; (RepTerms, RepTerms) --> (RepTerms, RepTerms)
  ;; (a) create reduce-terms
  ;;      1. Get gcd of numerator and denominator
  ;;      2. Multiply n and d by integerizing factor
  ;;      3. Divide f*n and f*d by gcd ==> n and d have integer coeffs!
  ;;      4. Divide terms of n and d by gcd of ALL coeffs
  (define (reduce-terms n d)
    (let* ((g (gcd-terms n d))
           (integerize (make-integerizer n d))
           (sloppy-n (car (div-terms (integerize n) g)))
           (sloppy-d (car (div-terms (integerize d) g)))
           (reduce-factor (abs (gcd (gcd-coeffs sloppy-n) 
                                    (gcd-coeffs sloppy-d))))
           (clean-n (car (apply-const div-terms sloppy-n reduce-factor)))
           (clean-d (car (apply-const div-terms sloppy-d reduce-factor))))
      ; (bkpt 'reduce-terms 'foo)
      (list clean-n clean-d)))

  ;; Reduce a rational polynomial to its lowest terms
  ;; (RepPoly, RepPoly) --> ({polynomial} X RepPoly, {polynomial} X RepPoly)
  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (let ((nd (reduce-terms (term-list p1)
                              (term-list p2))))
        (list (make-polynomial (variable p1) (car nd))
              (make-polynomial (variable p1) (cadr nd))))
      (error "Polys not in same var -- REDUCE-POLY"
             (list p1 p2))))

  ;; Multiply a term-list by a constant factor
  ;; (((RepTerms, RepTerms)-->RepTerms), Repterms, Sch-Num) --> RepTerm
  (define (apply-const proc terms c)
    (proc terms (adjoin-term (make-term 0 c)
                             (the-empty-termlist))))

  ;; Ex 2.87:
  ;; RepTerms --> Bool
  (define (=zero-poly? p) ; distinguish name from generic =zero?
    (let ((terms (term-list p)))
      (or (empty-termlist? terms)
          (all-coeffs-zero? terms))))

  ;; RepTerms --> Bool
  (define (all-coeffs-zero? terms)
    (if (empty-termlist? terms)
      #t
      (and (=zero? (coeff (first-term terms)))
           (all-coeffs-zero? (rest-terms terms)))))

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
  ;; Ex 2.91:
  ;; div returns a list of 2 polynomials (quotient remainder)
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (div-poly p1 p2)))
  (put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p))))
  ;; Ex 2.87:
  (put '=zero? '(polynomial) =zero-poly?)
  (put 'equ? '(polynomial polynomial) equ-poly?)
  ;; Ex 2.94:
  (put 'gcd '(polynomial polynomial)
       (lambda (a b) (tag (gcd-poly a b))))
  ;; Ex 2.97:
  (put 'reduce '(polynomial polynomial) reduce-poly)
  'done)

;;; Constructor
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;;; Run the procedure
(install-polynomial-package)
;;==============================================================================
;;==============================================================================
