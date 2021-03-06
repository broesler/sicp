;;==============================================================================
;;     File: ps5_exercises.scm
;;  Created: 12/02/2016, 13:06
;;   Author: Bernie Roesler
;;
;;  Description: PS5 Freshman Advisor Problems
;;
;;==============================================================================
(load "match.scm")
(load "myadv.scm")

;;; Pre-lab Exercise 1:
;;;   List-union checks for duplicates, append does not!
(list-union '(1 2 3) '(3 4 5)) ; Value: (1 2 3 4 5)
(append '(1 2 3) '(3 4 5))     ; Value: (1 2 3 3 4 5)

;;; Pre-lab Exercise 2:
;;;   Compute the product of all elements in a list using reduce
(reduce * 1 '(1 2 3 4 5)) ; Value: 120

;;; Pre-lab Exercise 3:
;;;   Use map + reduce to compute the sum of squares of a list
(reduce + 0 (map square '(3 4 5))) ; Value: 50

;;; Pre-lab Exercise 4:
;;;   What is returned by:
(reduce append '() '((1 2) (2 3) (4 6) (5 4)))      ; Value: (1 2 2 3 4 6 5 4)
(reduce list-union '() '((1 2) (2 3) (4 6) (5 4)))  ; Value: (1 2 3 6 5 4)

;;; Pre-lab Exercise 5:
;;; What does this procedure do?
(define (unique symbols)
  (reduce list-union
          '()
          (map list symbols)))
;;; This procedure returns a list of unique symbols, maybe call it "unique":
(unique '(x y z x t t y)) ; Value: (z x t y)

;------------------------------------------------------------------------------- 
;        Exercise 1: Expand (beginnings) and (general-advice)
;-------------------------------------------------------------------------------
;;;;;;;;;; Expanding (general-advice):
  ;;;
  ;;; 1 ]=> (see-advisor 'Bernie)
  ;;; (hi bernie)
  ;;; (i am your freshman advisor)
  ;;; (what are your plans for the semester)
  ;;; 
  ;;; ** (I do not know yet)
  ;;; (you do not know yet)
  ;;; 
  ;;; ** (What should I take?)
  ;;; (how about a freshman seminar)
  ;;; 
  ;;; ** (I like whiskey)
  ;;; (make sure to get time to explore the boston area)
  ;;; 
  ;;; ** (Give me whiskey)
  ;;; (i hear whiskey tasting is an excellent seminar.)
  ;;;

;;;;;;;;;; Expanding (beginnings):
  ;;;
  ;;; 1 ]=> (see-advisor 'Bernie)
  ;;; (hi bernie)
  ;;; (i am your freshman advisor)
  ;;; (what are your plans for the semester)
  ;;; 
  ;;; ** (I do not know yet)
  ;;; (you do not know yet)
  ;;; 
  ;;; ** (but I like to party)
  ;;; (i will not indulge you if you think that but you like to party)

;------------------------------------------------------------------------------- 
;        Exercise 2: trigger each rule 
;-------------------------------------------------------------------------------
;;;;;;;;;; All rules in (conventional-wisdom):
  ;;; ** (Should I take 6:001 this fall?)
  ;;; (6:001 is too much work for freshmen -- wait until next year)
  ;;; 
  ;;; ** (What about taking 8:01 instead?)
  ;;; (students really enjoy 8:01)
  ;;; 
  ;;; ** (I do not know which seminar I should take)
  ;;; (i hear that snorkeling in boston harbor is a really exciting seminar)
  ;;; 
  ;;; ** (I plan to take 2:25 next spring)
  ;;; (too bad -- 2:25 is not offered next spring)
  ;;; 
  ;;; ** (I would like to double major in booting and rallying)
  ;;; (booting is fascinating and you can make a living doing it if rallying does not work out)
  ;;; 
  ;;; ** (Is a double major a good idea?)
  ;;; (doing a double major is a lot of work)

;;;;;;;;;; All rules in (subject-knowledge):
  ;;; ** (what is 5:11 about)
  ;;; (5:11 is about smelly organic crud and goop)
  ;;; 
  ;;; ** (what are 8:01 and 8:02 about)
  ;;; (8:01 is about classical mechanics)
  ;;; (8:02 is about electricity and magnetism)
  ;;; 
  ;;; ** (how many units is 12:001)
  ;;; (12:001 is a 12 unit subject)
  ;;;
  ;;; ** (how many units are 8:01 and 6:001)
  ;;; (8:01 is a 12 unit subject)
  ;;; (6:001 is a 15 unit subject)
  ;;; 
  ;;; ** (what are the prerequisites for 8:03)
  ;;; (the prerequisites for 8:03 are 8:02 18:02)
  ;;; 
  ;;; ** (can i take 18:02)
  ;;; (the prerequisites for 18:02 are 18:01)

;------------------------------------------------------------------------------- 
;        Exercise 3: (all-prerequisites)
;-------------------------------------------------------------------------------
;; If subject is not in catalog => no prereqs
;; Need unique list
(define (all-prerequisites subject)
  (let ((entry (find subject catalog)))
    (if entry  ;; find returns entry or #f
      (let ((prereqs (entry-prerequisites entry)))
        (if (null? prereqs)
          '()
          (list-union prereqs 
                      (reduce list-union 
                              '() 
                              (map all-prerequisites prereqs)))))
      '())))

;;; Check: 
(all-prerequisites '12:004) 
; Value: (18:03 8:02 8:01 18:02 18:01)

;------------------------------------------------------------------------------- 
;        Exercise 4: add rule to (subject-knowledge)
;-------------------------------------------------------------------------------
;;;;;;;;;; Rule:
  ;;; (make-rule
  ;;;       `(can i take (? s1 ,in-catalog) if i have not taken (? s2 ,in-catalog))
  ;;;       (lambda (dict)
  ;;;         (let ((sub1 (entry-subject (value 's1 dict)))
  ;;;               (sub2 (entry-subject (value 's2 dict))))
  ;;;           (if (member sub2 (all-prerequisites sub1)) 
  ;;;             (write-line
  ;;;               (append '(no you cannot take)
  ;;;                       (list sub1)
  ;;;                       '(because)
  ;;;                       (list sub2)
  ;;;                       '(is a prerequisite)))
  ;;;             (write-line '(sure you can take it!))))))

;;;;;;;;;; Transcript:
  ;;; ** (can i take 7:012 if i have not taken 8:01)
  ;;; (sure you can take it!)
  ;;; 
  ;;; ** (can i take 12:004 if i have not taken 8:01)
  ;;; (no you cannot take 12:004 because 8:01 is a prerequisite)

;------------------------------------------------------------------------------- 
;        Exercise 5: check for circular prereqs
;-------------------------------------------------------------------------------
;;; Take a list of subjects, and determine if any of the subjects are
;;; prerequisites of any other subject in the list. 
;;; Return false if there are circular prereqs, true otherwise.
(define (check-circular-prerequisites? subjects)
  (null? (list-intersection subjects
                            (reduce list-union 
                                    '() 
                                    (map all-prerequisites subjects)))))

(check-circular-prerequisites? '(8:01 5:11)) ; Value: true
(check-circular-prerequisites? '(18:01 12:004)) ; Value: false

;------------------------------------------------------------------------------- 
;        Exercise 6: Total units
;-------------------------------------------------------------------------------
(define (total-units subjects)
  (reduce + 
          0 
          (map (lambda (x) (entry-units (find x catalog))) 
               subjects)))

(total-units '(6:001 18:01 8:01)) ; Value: 39

;------------------------------------------------------------------------------- 
;        Exercise 7: check subject list for issues
;-------------------------------------------------------------------------------
(define (check-subject-list subjects)
  (cond ((> (total-units subjects) 54)
         (write-line '(these subjects exceed the freshman credit limit!)))
        ((not (check-circular-prerequisites? subjects))
         (write-line '(there are circular prereqs in your list!)))
        (else
          (let ((prereqs (reduce list-union
                                 '()
                                 (map immediate-prereqs subjects))))
            (if (null? prereqs)
              (write-line '(great!))
              (write-line 
                (append '(you need the following prerequisites:)
                        prereqs)))))))

;;; Get the first level of prereqs of a subject
(define (immediate-prereqs subject)
  (let ((entry (find subject catalog)))
    (if (null? entry) 
      '()
      (entry-prerequisites entry))))

;;; Test code:
(immediate-prereqs '12:004) ; Value: (18:03 8:02)
(check-subject-list '(12:004 5:11))
; Value: (you need the following prerequisites: 18:03 8:02 a-strong-stomach) 
(check-subject-list '(6:001 18:01 8:01 7:012 5:11)) 
; Value: (these subjects exceed the freshman credit limit!)
(check-subject-list '(12:004 8:01))

;------------------------------------------------------------------------------- 
;        Exercise 8: rule to subject-knowledge
;-------------------------------------------------------------------------------
;;;;;;;;;; Rule:
  ;;; (make-rule
  ;;;   `(i want to take (?? s ,subjects))
  ;;;   (lambda (dict)
  ;;;     (check-subject-list (map entry-subject (value 's dict)))))

;;;;;;;;;; Transcript:
  ;;; ** (i want to take 12:004 and 5:11)
  ;;; (you need the following prerequisites: 18:03 8:02 a-strong-stomach)
  ;;; 
  ;;; ** (i want to take 6:001 18:01 8:01 7:012 and 5:11)
  ;;; (these subjects exceed the freshman credit limit!)
  ;;; 
  ;;; ** (i want to take 12:004 and 8:01)
  ;;; (there are circular prereqs in your list!)
  ;;; 
  ;;; ** (i want to take 8:01)
  ;;; (great!)

;------------------------------------------------------------------------------- 
;        Exercise 9: ask about subject descriptions
;-------------------------------------------------------------------------------
;;;;;;;;;; Rule:
  ;;; (make-rule
  ;;;       '(is there a course about (?? x))
  ;;;       (lambda (dict)
  ;;;         (let ((topic (value 'x dict)))
  ;;;           (write-line (append '(you asked about) topic))
  ;;;           (let ((matches (filter (in-description? topic) catalog)))
  ;;;             (if (null? matches)
  ;;;               (write-line '(no there is not))
  ;;;               (for-each (lambda (entry)
  ;;;                           (write-line
  ;;;                             (append '(the course)
  ;;;                                     (list (entry-subject entry))
  ;;;                                     '(is about)
  ;;;                                     (entry-summary entry))))
  ;;;                         matches))))))
  ;;;
  ;;; ;;; Auxiliary procedure for Ex 9 rule
  ;;; (define (in-description? topic)
  ;;;   (lambda (entry) 
  ;;;     (member topic (entry-summary entry))))


;;;;;;;;;; Transcript:
  ;;; ** (is there a course about magnetism)
  ;;; (the course 8:02 is about electricity and magnetism)
  ;;; 
  ;;; ** (is there a course about diseases)
  ;;; (the course 7:012 is about diseases and their applications)
  ;;; (the course 7:013 is about diseases and their applications)
  ;;; (the course 7:014 is about diseases and their applications)

;;==============================================================================
;;==============================================================================
