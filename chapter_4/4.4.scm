; -------------------------------------------------------------------
; Special form
(define (eval-and exp env)
    (define (iterate exps)
        (cond ((last-exp? exps) (true? (first-exp exps)))
              ((false? (eval (first-exp exps) env)) 'false)
              (else (eval-and (rest-exps exps) env))))
    (iterate (cdr exp))

(define (eval-or exp env)
    (define (iterate exps)
        (cond ((last-exp? exps) (true? (first-exp exps)))
              ((true? (eval (first-exp exp) env)) 'true)
              (else (eval-or (rest-exps exps) env))))
    (iterate (cdr exp)))

(put 'operation 'and eval-end)
(put 'operation 'or eval-or)

; -------------------------------------------------------------------
; Derived expressions

; accepts if expression with **AND** in predicate
(define (and->if exp)
    (define (iterate exps)
        (if (last-exp? exps)
            (make-if (first-exp exps)
                     (if-consequent exp)
                     (if-alternative exp))
            (make-if (first-exp exps)
                     (iterate (rest-exps exps))
                     (if-alternative exp))))
    (iterate (if-predicate exp)))

; accepts if expression with **OR** in predicate
(define (or-if exp)
    (define (iterate exps)
        (if (last-exp? exps)
            (make-if (first-exp exps)
                     (if-consequent exp)
                     (if-alternative exp))
            (make-if (first-exp exps)
                     (if-consequent exp)
                     (iterate (rest-exps exps)))))
    (iterate (if-predicate exp)))

