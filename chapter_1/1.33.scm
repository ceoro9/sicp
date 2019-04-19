(define (filtered-accumulate combiner null-value filter-func term a next b)
    (define (cal-result x result)
        (if (filter-func x) 
            (combiner (term x) result)
            result
        ))
    
    (define (iter v result)
        (if (> v b)
            result
            (iter (next v) (cal-result v result))
        ))

    (iter a null-value))

; sum of positive number in interval
(define (positive? x) (> x 0))
(define (inc x) (+ x 1))
(define (combiner a b) (+ a b))
(define (val x) x)
(display (filtered-accumulate combiner 0 positive? val -5 inc 5))
(newline)


; a) sum squares of prime numbers on interval
(define (prime? x) 

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

    (define (sqrt n) (sqrt-iter 1 n))
    
    (define (next-divisor x)
        (cond ((= x 2) 3)
            (else (+ x 2)))) 

    (define (handle-prime n i)
        (cond ((> i (sqrt n) 1))
        (else (cond ((= (remainder n i) 0) #f)
              (else (handle-prime n (next-divisor i)))))))

    (handle-prime x 2))

(define (square a) (* a a))
(display (filtered-accumulate + 0 prime? square 4 inc 9))
(newline)


; b) mul of numbers which gcd(i, n) = 1

(define n 7)

(define (max a b)
    (cond ((< a b) b)
    (else a)))

(define (min a b)
    (cond ((= (max a b) a) b)
    (else a)))

(define (smallest-divisor-handle a b)
    (cond ((= b 0) a)
    (else (smallest-divisor-handle b (remainder a b)))))

(define (smallest-divisor a b)
    (smallest-divisor-handle (min a b) (max a b)))

(define (filter-func x) (= (smallest-divisor x n) 1))
(define (val x) x)
(define (inc x) (+ x 1))

(display (filtered-accumulate + 0 filter-func val 0 inc 20))
