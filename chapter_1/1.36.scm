(define inflecity 0.001)

(define (average a b)
    (/ (+ a b) 2))

(define (abs x)
    (if (> x 0) 
        x
        (* -1 x)))

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) inflecity))
    (define (try guess)
        (display guess)
        (newline)
        (let ((next (f guess))) 
            (if (close-enough? next guess) 
                next
                (try next))))
    (try first-guess))


(display "Simple fixed point method:")
(newline)
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
(newline)
(newline)
(display "Fixed point with average dump")
(newline)
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)
