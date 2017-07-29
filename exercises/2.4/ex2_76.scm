;;==============================================================================
;;     File: ex2_76.scm
;;  Created: 11/27/2016, 20:05
;;   Author: Bernie Roesler
;;
;;  Description: Summary of generic operations strategies
;;
;;==============================================================================

;;; 1. generic operations with explicit dispatch
;;;     -- selectors are generic (cond statements inside)
;;;     -- generic interface procedures must know about all implementations, and
;;;     we must change them (add a cond) each time we add a new implementation
;;; 2. data-directed style
;;;     -- each operation dispatches on data type
;;;     -- to add a new type, we must make a new "install" procedure and define
;;;     the relevant operations there, along with the addition of a column in
;;;     the operation-and-type table
;;;     -- to add a new operation, we just add it to each type, and add the row
;;;     into the table
;;;     -- (cond) is essentially in each (get ...), which is defined in each
;;;     external operation 
;;;         ==> most work required to add operation
;;; 3. message-passing-style
;;;     -- each data object (constructor) dispatches on operation name
;;;     -- (cond) is in each data object, instead of each operation 
;;;         ==> most work required to add data type
;;;     -- to add a new type, we create a new constructor 

;;; If new TYPES are often added, we want to use: data-directed
;;; If new OPERATIONS are often added, we want to use: message-passing

;;==============================================================================
;;==============================================================================
