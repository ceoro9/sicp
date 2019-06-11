(define (equal? list1 list2)
    (cond ((AND (NOT (pair? list1))
                (NOT (pair? list2)))
                (eq? list1 list2))
          ((AND (pair? list1) 
                (pair? list2))
                (AND (equal? (car list1) (car list2))
                     (equal? (cdr list1) (cdr list2))))
          (else #f)))




(display (equal? '(1 2) '(1 2)))
