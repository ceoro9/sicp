(define (install-scheme-number-package)
    ; ...

    (put 'equ? '(scheme-number scheme-number) =))

(define (install-rational-number-package)
    ; ...

    (define (equ? x y)
        (and
            (= (numer x) (numer y))
            (= (denom x) (denom y))))

    (put 'equ? '(rational rational) equ?) )

(define (install-complex-number-package)
    ; ...

    (define (equ? x y)
        (and
            (= (real-part x) (real-part x))
            (= (imag-part x) (imag-part y))))

    (put 'equ '(complex complex) equ?))


(define (equ? a b)
    (if (not (eq? (type-tag a)
                  (type-tag b)))
        (error "equ? params are uncomparable, because they have different types.")
        (apply-generic 'equ? a b)))

