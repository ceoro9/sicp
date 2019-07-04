(define (adjoin-term term term-list)
    (define (handle current-order terms)
        (if (= (order term) current-order)
            (cons (order term) terms)
            (else (handle (+ current-order 1) (cons 0 terms)))))
    
    (if (zero? term)
        term-list
        (handle (length term-list) term-list)))

(define (the-empty-termlist) '())

(define (first-term term-list)
    (make-term (car term-list)
               (- (length term-list) 1)))

(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

