(define (last-pair l)
    (if (null? (cdr l))
        (car l)
        (last-pair (cdr l))))

(display (last-pair (list 1 2 3 4 5)))
(newline)
(display (last-pair (list 1)))
