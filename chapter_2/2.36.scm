(define (accumulate p start-value sequance)
    (define (iter c result)
        (if (null? c)
            result
            (iter (cdr c)
                  (p result (car c)))))
    (iter sequance start-value))

(define (first-elements-of-sequances seq)
    (map (lambda (s) (car s)) seq))

(define (sequances-without-first-element seq)
    (map (lambda (s) (cdr s)) seq))

(define (accumulate-n p initial seq)
    (if (null? (car seq))
        '()
        (cons (accumulate p initial (first-elements-of-sequances seq))
              (accumulate-n p initial (sequances-without-first-element seq)))))

(display (accumulate-n (lambda (a b) (+ a b)) 0 (list (list 1 2 3) (list 4 5 6))))

