;;==============================================================================
;;     File: ex2_67-70.scm
;;  Created: 11/21/2016, 13:23
;;   Author: Bernie Roesler
;;
;;  Description: Huffman encoding exercises 
;;
;;==============================================================================
(load "huffman.scm")
(load "set-ordered.scm") ; for element-of-set?

;------------------------------------------------------------------------------- 
;        Ex 2.67 -- decode a message
;-------------------------------------------------------------------------------
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define decoded-message (decode sample-message sample-tree))
(printval decoded-message) ; Value: (A D A B B C A)

;------------------------------------------------------------------------------- 
;        Ex 2.68 -- encoding a message
;-------------------------------------------------------------------------------
(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

;;; encode-symbol returns a list of bits
(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((not (symbol-in-list? symbol (symbols tree)))
         (error "symbol not found -- ENCODE-SYMBOL" symbol))
        (else
          (let ((lb (left-branch tree))
                (rb (right-branch tree)))
            (if (symbol-in-list? symbol (symbols lb))
              (cons 0 (encode-symbol symbol lb))
              (cons 1 (encode-symbol symbol rb)))))))

;;; Is the symbol in the list? (i.e. (memq 'a (a b c)) => #t)
(define (symbol-in-list? symbol symbols)
  (if (memq symbol symbols) #t #f))

;;; Test code:
; (printval (encode-symbol 'A sample-tree)) ; Value: (0)
; (printval (encode-symbol 'B sample-tree)) ; Value: (1 0)
; (printval (encode-symbol 'C sample-tree)) ; Value: (1 1 1)
; (printval (encode-symbol 'D sample-tree)) ; Value: (1 1 0)
(define encoded-message (encode decoded-message sample-tree))
(printval encoded-message) ; Value: (0 1 1 0 0 1 0 1 0 1 1 1 0)
(printval (equal? encoded-message sample-message)) ; Value: #t
;;==============================================================================
;;==============================================================================
