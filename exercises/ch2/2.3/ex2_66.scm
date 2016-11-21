;;==============================================================================
;;     File: ex2_66.scm
;;  Created: 11/21/2016, 12:23
;;   Author: Bernie Roesler
;;
;;  Description: lookup function for trees
;;
;;==============================================================================
(load "ex2_63-65.scm")

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))))

;;==============================================================================
;;==============================================================================
