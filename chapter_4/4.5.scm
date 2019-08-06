(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
(define (cond-arrow-clause? clause)
    (eq? (car (cond-actions clause)) '=>))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
    (if (null? clauses)
        'false
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (cond ((cond-else-clause? first)
                 (if (null? rest)
                     (sequence->exp (cond-actions first))
                     (error "ELSE clause isn't last: COND->IF"
                            clauses)))
                ; cond-clause is going to be evaluated 2 times,
                ; so it should be a clean function to achive
                ; transparrent programm behavior. To get rid of
                ; such behavior, let operator should be introduced
                ; to store a value of cond-clause expression.
                ((cond-arrow-clause? first)
                 (make-if (cond-predicate first)
                          (list (cadr (cond-actions first))
                                      (cond-predicate first))
                          (expand-clauses rest)))
                (else (make-if (cond-predicate first)
                               (sequence->exp (cond-actions first))
                               (expand-clauses rest)))))))

