;; Newton's method to find sqrt

(define infelicity 0.01)

(define (square x) (* x x))

(define (abs x) (if (< x 0) (* -1 x) x))

(define (is-good-enough? guess x)(< (abs (- (square guess) x)) infelicity))

(define (average x y) (/ (+ x y) 2))

(define (improve guess x) (average guess (/ x guess)))

(define (sqrt-iter guess x)
	(if (is-good-enough? guess x)
    guess
		(sqrt-iter (improve guess x) x)))

(define (sqrt x) (sqrt-iter 1 x))

(define (sqrt-tiny-iter guess-prev guess-current x)
	(if (< (abs (- guess-current guess-prev)) infelicity) 
		guess-current
		(sqrt-tiny-iter guess-current (improve guess-current x) x)))

(define (sqrt-tiny x) (sqrt-tiny-iter 1 (improve 1 x) x))

(display (sqrt 0.4224))
(display ("\n"))
(display (sqrt-tiny 0.4224))

