(define (same-parity x . w)
    (define (even? x) (= (remainder x 2) 0))
    
    (define (same-parity? a b)
        (if (even? a)
            (even? b)
            (NOT (even? b))))

    (define (same-parity-iter current)
        (if (null? current)
            '()
            (if (same-parity? x (car current))
                (let ((el (car current))
                      (l (same-parity-iter (cdr current))))
                    (cons el l))
                (same-parity-iter (cdr current)))))

    (cons x (same-parity-iter w)))


(display (same-parity 1 2 3 4 5 6 7))
(newline)
(display (same-parity 2 3 4 5 6 7))
