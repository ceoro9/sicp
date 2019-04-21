(define (repeated f n)
    (define (iter i result)
        (if (> i n)
            result
            (iter (+ i 1) (f result))))
    (lambda (x)
        (iter 2 (f x))))

(define (square x) (* x x))

(display ((repeated square 2) 5))
