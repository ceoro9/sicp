(define (accumulate combiner null-value term a next b)
    (define (iter v result)
        (if (> v b)
            result
            (iter (next v) (combiner (term v) result))
        ))
    (iter a null-value))


(define (sum a b)
    (define (inc x) (+ x 1))
    (define (combiner a b) (+ a b))
    (define (val x) x)

    (accumulate combiner 0 val a inc b))

(define (product a b)
    (define (inc x) (+ x 1))
    (define (combiner a b) (* a b))
    (define (val x) x)

    (accumulate combiner 1 val a inc b)
)


(display (sum 1 10))
(newline)
(display (product 1 5))
