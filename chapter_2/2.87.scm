(define accumulate (op base func ls)
    (if (null? ls)
        base
        (op (func (car ls))
            (accumulate op base func (cdr ls)))))

(define (install-polymonial-package)
    
    ; ...

    (define (poly-zero? p)
        (accumulate
            #t
            (lambda (a b) (and a b))
            (lambda (a) (zero? a)) ; call to generic function
            (term-list p)))

    (put 'zero? '(polynomial) poly-zero?))

