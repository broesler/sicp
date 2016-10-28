; true if any element of lst satisfies func?
(define (any? func? lst)
    (cond ((null? lst) #f)
          ((func? (car lst)) #t)
          (else (any? func? (cdr lst)))))
