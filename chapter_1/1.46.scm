(define (iterative-improve is-good-enough? improve)
    (define (cal-iter current-value)
        (if (is-good-enough? current-value)
            current-value
            (cal-iter (improve current-value))))
    
    (lambda (start-value) (cal-iter start-value)))


(define (average a b)
    (/ (+ a b) 2.0))

(define (square x)
    (* x x))

(define (sqrt x)
    (define inflecity 0.0001)

    (define (improve guess)
        (average guess (/ x guess)))

    (define (is-good-enough? guess)
        (< (abs (- (square guess) x)) inflecity))

    ((iterative-improve is-good-enough? improve) 1))


(define (fixed-point f)
    (define inflecity 0.0001)

    (define (is-good-enough? guess)
        (< (abs (- guess (f guess))) inflecity))

    (define (improve x) (f x))

    ((iterative-improve is-good-enough? improve) 1))


(display (sqrt 36))
(newline)
(display (fixed-point cos))
