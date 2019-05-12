(define (accumulate p start-value sequance)
    (define (iter c result)
        (if (null? c)
            result
            (iter (cdr c)
                  (p result (car c)))))
    (iter sequance start-value))

(define (map p sequance)
    (if (null? sequance)
        '()
        (accumulate (lambda (acc current-value) 
                       (append acc
                               (list (p current-value))))
                    (car sequance)
                    (cdr sequance))))

(define (append seq1 seq2)

(define (length seq)
    (accumulate (lambda (result _) (+ result 1)) 0 seq))

(display (map (lambda (a) (* a 2)) (list 1 2 3)))

(newline)

(display (append (list 1 2) (list 3 4)))

(newline)

(display (length (list 1 2 3)))
