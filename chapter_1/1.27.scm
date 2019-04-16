(define (even? x) (= (remainder x 2) 0))

(define (expmod a n m)
    (cond ((= n 0) 1)
          ((even? n) (remainder (square (expmod a (/ n 2) m)) m))
          (else (remainder (* a (expmod a (- n 1) m)) m))))

(define (ferma-test x)
    (define (try-it a)
        (= a (expmod a x x)))
    (try-it (+ 1 (random (- x 1)))))

(define (check-number-iter a i flag)
    (cond ((= i a) #t)
          ((NOT flag) #f)
          (else (check-number-iter a (+ i 1) (ferma-test a)))))

(define (check-number a) (check-number-iter a 1 #t))

(display (check-number 561))
(display (check-number 1105))
(display (check-number 1729))
(display (check-number 2465))
(display (check-number 2821))
(display (check-number 6601))
