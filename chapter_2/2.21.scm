(define (map proc l)
    (if (null? l)
        '()
        (cons (proc (car l))
              (map proc (cdr l)))))

(define (square x) (* x x))

(define (square-list items)
    (if (null? items)
        '()
        (cons (square (car items)) (square-list (cdr items)))))

(define (map-square-list items)
    (map (lambda (x) (* x x)) items))

(display (square-list (list 1 2 3 4 5)))
(newline)
(display (map-square-list (list 1 2 3 4 5)))
