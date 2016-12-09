;;==============================================================================
;;     File: pattern_matching_notes.scm
;;  Created: 12/02/2016, 16:51
;;   Author: Bernie Roesler
;;
;;  Description: Notes from Lecture 4A on pattern matching
;;
;;==============================================================================
;;; Process:
;;;                  Rule
;;; Pattern ----------------------> Skeleton
;;;    |                                |
;;;    |                                |
;;;    | Match                          | Instantiation
;;;    |                                |
;;;    |                                |
;;;    v                                v
;;; Expression -------------------> Expression
;;;   source                          target
;;;
;;; pattern == something that matches
;;; skeleton == something you substitute into to get an expression
;;; ** match a pettern to get an expression, then instantiate a skeleton to get
;;; a target expression
;;;
;;; Pattern match: 
;;; foo     -- matches exactly foo
;;; (f a b) -- matches a lit whose first element is f, 2nd is a, 3rd is b
;;; (? x)   -- matches anything, call it x
;;; (?c x)  -- matches a constant, call it x
;;; (?v x)  -- matches a variable, call it x
;;; 
;;; Patterns are matched element-by-element, but some may match these special
;;; syntactic variables
;;; 
;;; Skeletons for instantiation: 
;;; foo     -- instantiates to itself
;;; (f a b) -- instantiates to a 3-list which are the result of instantiating
;;;            each of f, a, and b
;;; (: x)   -- instantiates to the value of x, as in the matched pattern

;;; Simplifier
(define dsimp
  (simplifier deriv-rules))

;;; => (dsimp '(dd (+ x y) x))
;;; (+ 1 0)

;;; We have a deck of rules [][][][][]
;;; Each rule has a pattern and a skeleton 
;;;     [Rule | P , S]
;;;             |   \
;;;             |    \ 
;;;             |     `------------------,
;;;             v    (Dictionary)        v
;;;  ---->[matcher]---------------->[instantiator]---
;;;  |                                              |
;;;  |              (Expression)                    |
;;;  |______________________________________________|
;;;
;;; Iterate until parts being passed around are all the same!

;;; Pattern matcher:
;;;                   Pattern
;;;                      |
;;;                      v
;;;                 _____________
;;;                |             |
;;; Expression --->|   Matcher   |---> Dictionary
;;;                |_____________|
;;;                      ^
;;;                      |
;;;                  Dictionary
;;; 
;;; We are comparing two trees: (1) Expression (2) Pattern
;;; Pattern: (+ (* (? x) (? y)) (? y))
;;; Expression: (+ (* 3 x) x) ; matches! x gets 3, y gets 'x

;;;                   Skeleton
;;;                      |
;;;                      v
;;;                 _______________
;;;                |              |
;;; Dictionary --->| Instantiator |---> Expression
;;;                |______________|
;;;
;;; Recursive tree

;;; Simplifier:
;;; GIGO (garbage in, garbage out)
;;==============================================================================
;;==============================================================================
