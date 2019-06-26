(define (install-scheme-number-package)
    ; ...

    (put 'zero? '(scheme-number) (lambda (x) (= x 0))))

(define (install-rational-number-package)
    ; ...

    (define (zero? x) (= (numer x) 0))

    (put 'equ? '(rational) zero?))

(define (install-complex-number-package)
    ; ...

    (define (zero? x)
        (and
            (= (real-part x) 0)
            (= (imag-part x) 0)))

    (put 'equ '(complex) zero?))

(define (zero? x) (apply-generic 'zero? x))

