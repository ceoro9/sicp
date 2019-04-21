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
        (let ((next (f guess))) 
            (if (close-enough? next guess) 
                next
                (try next))))
    (try first-guess))


(define (golden-ration)
    (fixed-point (lambda (x) (average x (+ 1 (/ 1 x)))) 1.0))

(display (golden-ration)) ; average dump
(newline)
(display (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.9))
