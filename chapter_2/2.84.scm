(define (accumulate op base func ls)
    (if (null? ls)
        base
        (op (func (car ls))
            (accumulate op base func (cdr ls)))))

(define types-list '(integer rational real complex))

(define (higher? type-1 type-2)
    
    (define (handle t-list)
        (if (null? t-list)
            (error "No such types in list" (list type-1 type-2))
            (cond ((equal? type-1 (car t-list)) #f)
                  ((equal? type-2 (car t-list)) #t)
                  (else (handle (cdr t-list))))))

    (handle types-list))

(define (apply-generic op . args)
    (accumulate
        (lambda (a b)
            (if (higher? (type-tag a) (type-tag b))
                (((get op
                       (list (type-tag a)
                             (type-tag a))) 
                     a
                     (raise b (type-tag a))))
                (((get op
                       (list (type-tag b)
                             (type-tag b))) 
                     b
                     (raise a (type-tag b))))))
        (car args)
        (lambda (x) x)
        (cdr args)))
