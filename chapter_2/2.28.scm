(define (fringe tree)
    (define (iter c result)
        (if (null? c)
            result
            (if (pair? (car c)) 
                (iter (cdr c)
                      (iter (car c) result))
                (iter (cdr c)
                      (append result (list (car c)))))))
    (iter tree '()))

(display (fringe (list (list 1 (list 2 (list 3))) (list 4 5))))
