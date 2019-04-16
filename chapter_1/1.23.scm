;; different next divisor implementation

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

(define (handle-prime n i)
    (cond ((> i (sqrt n) 1))
          (else (cond ((= (remainder n i) 0) #f)
                      (else (handle-prime n (next-divisor i)))))))

(define (prime? n)
    (handle-prime n 2)) 

(define (timed-prime-test n)
    (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
    (cond ((prime? n) (report-prime (- (runtime) start-time) n))
    (else #f)))

(define (report-prime elapsed-time prime)
    (newline)
    (display " *** ")
    (newline)
    (display prime) 
    (display " *** ")
    (newline)
    (display elapsed-time)
    (newline)
    #t)

(define (next-divisor x)
    (cond ((= x 2) 3)
        (else (+ x 2)))) 

(define (handle-search-for-primes n i counter)
    (cond ((= i (+ n 1)) 0)
          (else (begin
                    (timed-prime-test i)
                    (handle-search-for-primes n (next-divisor i) (+ counter 1)
                )))))

(define (search-for-primes n)
    (handle-search-for-primes n 2 0))

(display (search-for-primes 30))
