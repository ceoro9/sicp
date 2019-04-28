(define (cons x y)
    (lambda (z) (z x y)))

(define (car z)
    (z (lambda (p q) p)))

(define (cdr z)
    (z (lambda (p q) q)))

(display (cdr (cons 5 10)))
