(define (reverse l)
    (define (reverse-iter current result)
        (if (null? current)
            result
            (reverse-iter (cdr current)
                          (cons (car current) result))))
    (reverse-iter l '()))

(define (deep-reverse l)
    (define (iter c result)
        (if (null? c)
            result
            (if (list? (car c))
                (iter (cdr c)
                      (cons (deep-reverse (car c)) result))
                (iter (cdr c)
                      (cons (car c) result)))))
    (iter l '()))

(define a (list (list 3 (list 9 10)) (list 230 (list 5 6)) 3 4 5))
(define b (list 1 2 3))

(display a)
(newline)
(display (deep-reverse a))

(newline)
(newline)

(display b)
(newline)
(display (deep-reverse b))

