(define (subsets s)
    (if (null? s)
        (list '())
        (let ((rest (subsets (cdr s))))
            (append rest (map (lambda (el)
                                  (cons (car s) el)) 
                         rest)))))

(display (subsets (list 1 2 3)))
