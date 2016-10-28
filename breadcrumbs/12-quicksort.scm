;input a pivot point and a list
;return a list of 3 list '((elements < pivot) (elements = to pivot) (elements > pivot))
(define (qs-filter pivot ls)
  (define (h ls less equal great)
    (cond ((= (length ls) 0) (list less equal great))
          ((< (car ls) pivot) (h (cdr ls) (cons (car ls) less) equal great))
          ((= (car ls) pivot) (h (cdr ls) less (cons (car ls) equal) great))
          (else (h (cdr ls) less equal (cons (car ls) great)))))
  (h ls '() '() '()))

(define (qs ls)
  (if (or (null? ls) (= (length ls) 1))
      ls ;; base case
      (let ((pivot (list-ref ls (random (length ls))))) ;; selects a pivot element
      (let ((result (qs-filter pivot ls)))  ;; calls qs-filter which splits the list arount the piviot element 
      (append (qs (car result)) (append (car (cdr result)) (qs (car (cdr (cdr result)))))))))) 

; makes a random list of with each random element is in range 0 to (- up 1)
; used to make list to run quciksort on
(define (make-random-list up size)
  (define (h size result)
    (if (= size 0) 
        result
        (h (- size 1) (cons (random up) result))))
  (h size '()))