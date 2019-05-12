(define (fold-left p start-value sequance)
    (define (iter c result)
        (if (null? c)
            result
            (iter (cdr c)
                  (p (car c) result))))
    (iter sequance start-value))

(define (fold-right op initial sequence) 
    (if (null? sequence) 
        initial 
    (op (car sequence) 
        (fold-right op initial (cdr sequence))))) 




(define (fold-right-reverse sequence)
    (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (fold-left-reverse sequence)
    (fold-left (lambda (x y) (append (list x) y)) '() sequence))


 
(display (fold-right-reverse (list 1 3 5)))
(newline)
(display (fold-left-reverse (list 1 3 5)))
