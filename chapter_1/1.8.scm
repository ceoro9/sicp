;; Newton's method to find 1^3

(define infelicity 0.01)

(define (cube x) (* x x x))

(define (square x) (* x x))

(define (abs x) (if (< x 0) (* -1 x) x))

(define (is-good-enough? guess x)(< (abs (- (cube guess) x)) infelicity))

(define (average x y) (/ (+ x y) 2))

(define (improve guess x) (average guess (/ (+ (/ x (square guess)) (* 2 guess)) 3)))

(define (cubic-iter guess x)
	(if (is-good-enough? guess x)
    guess
		(cubic-iter (improve guess x) x)))

(define (cubic x) (cubic-iter 1 x))

(define (cubic-tiny-iter guess-prev guess-current x)
	(if (< (abs (- guess-current guess-prev)) infelicity) 
		guess-current
		(cubic-tiny-iter guess-current (improve guess-current x) x)))

(define (cubic-tiny x) (cubic-tiny-iter 1 (improve 1 x) x))

(display (cubic 10))
(display "\n")
(display (cubic-tiny 10))

(display "\n")

(display (cubic 0.4224))
(display "\n")
(display (cubic-tiny 0.4224))

