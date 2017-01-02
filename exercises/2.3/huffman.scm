;;==============================================================================
;;     File: huffman.scm
;;  Created: 11/21/2016, 13:13
;;   Author: Bernie Roesler
;;
;;  Description: Huffman encoding
;;
;;==============================================================================

;------------------------------------------------------------------------------- 
;        Leaves
;-------------------------------------------------------------------------------
;;; Leaf Constructor:
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

;;; Predicate:
(define (leaf? object)
  (eq? (car object) 'leaf))

;;; Selectors:
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;------------------------------------------------------------------------------- 
;        Trees
;-------------------------------------------------------------------------------
;;; Tree Constructor:
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

;;; Tree Selectors:
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;------------------------------------------------------------------------------- 
;        Decoding
;-------------------------------------------------------------------------------
;;; (0's and 1's, tree) -> list
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;------------------------------------------------------------------------------- 
;        Sets of weighted elements
;-------------------------------------------------------------------------------
;;; Ordered set of leaves and trees by increasing weight
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

;;==============================================================================
;;==============================================================================
