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

(define (deriv g)
    (define dx 0.001)
    (lambda (x)
        (/ (- (g (+ x dx)) (g x)) 
            dx)))

(define (cube x) (* x x x))

(define (newton-transform g)
    (lambda (x)
        (- x (/ (g x) ((deriv g) 
            x)))))

(define (newtons-method g guess)
    (fixed-point (newton-transform g) guess))

; ... testing ...
(newline)
(display (newtons-method cos 1.0)) ; cos x = 0; x = Pi/2
(newline)

(define (cubic a b c)
    (define (cube x) (* x x x))
    (define (square x) (* x x))

    (lambda (x)
        (+ 
            (cube x) 
            (* a (square x)) 
            (* b x) 
            c
        )))

(newline)
(display (newtons-method (cubic 1 2 3) 1.0))
(newline)






