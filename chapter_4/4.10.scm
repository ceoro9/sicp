; ((()) ... cond),
; instead of (cond (()) ...)
(define (cond? exp) (equal? (cadr 'cond)))
(define (cond-clauses exp) (car exp))

