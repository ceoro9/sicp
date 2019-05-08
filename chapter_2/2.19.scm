(define (reverse l)
    (define (reverse-iter current result)
        (if (null? current)
            result
            (reverse-iter (cdr current)
                          (cons (car current) result))))
    (reverse-iter l '()))

(define (except-first-denomination l) (cdr l))

(define (first-denomination l) (car l))

(define (no-more? l) (null? l))

(define (cc amount coin-values)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else
            (+ (cc amount
                   (except-first-denomination coin-values))
               (cc (- amount
                     (first-denomination coin-values))
                   coin-values)))))

(display (cc 100 (list 50 25 10 5 1)))
(newline)
(display (cc 100 (list 25 50 10 5 1)))
(newline)
(display (cc 100 (reverse (list 25 50 10 5 1))))
