(define (reverse l)
    (define (reverse-iter current result)
        (if (null? current)
            result
            (reverse-iter (cdr current)
                          (cons (car current) result))))
    (reverse-iter l '()))

(display (reverse (list 1 2 3 4 5)))
(newline)
(display (reverse (list 10)))
(newline)
(display (reverse ()))
