(define (f x) x)

(define (g x) x)

(define (compose f1 f2)
    (lambda (x)
        (f1 (f2 x))))

(define (square x) (* x x))

(define (inc x) (+ x 1))

(display ((compose square inc) 6))
