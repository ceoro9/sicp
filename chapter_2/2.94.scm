(define (install-polynomial-package)
    
    (define (gcd-poly P1 P2)
       (make (variable P1)
             (gcd-terms (term-list P1)
                        (term-list P2)))

    (define (greatest-common-divisisor P1 P2)
        (if (same-variable? P1 P2)
            (gcd-poly P1 P2)
            (error "To apply this operation polynomials should have the same variable")))

    (put
        'greatest-common-divisor
        '(polynomial polynomial)
        greatest-common-divisor))

(define (greatest-common-divisor p1 p2)
    (apply-generic 'greatest-common-divisor p1 p2))

