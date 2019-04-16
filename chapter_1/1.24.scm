(define ferma-test-try-times 5)

(define (even? x) (= (remainder x 2) 0))

(define (expmod a n m)
    (cond ((= n 0) 1)
          ((even? n) (remainder (square (expmod a (/ n 2) m)) m))
          (else (remainder (* a (expmod a (- n 1) m)) m))))

(define (ferma-test x)
    (define (try-it a)
        (= a (expmod a x x)))
    (try-it (+ 1 (random (- x 1)))))

(define (fast-prime? x times)
    (cond ((= times 0) #t)
          ((ferma-test x) (fast-prime? x (- times 1)))
          (else #f)))

(define (prime? n) (fast-prime? n ferma-test-try-times))

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


(search-for-primes 20)
