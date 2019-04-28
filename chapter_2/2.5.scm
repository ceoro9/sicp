(define (even? x)
    (= (remainder x 2) 0))

(define (square x)
    (* x x))

(define (power b n)
    (cond ((= n 0) 1)
          ((even? n) (square (power b (/ n 2))))
          (else (* b (power b (- n 1))))))

(define (count-power n divisor)
    (define (iter x)
        (if (= 0 (remainder n (power divisor x)))
            (iter (+ x 1))
            (- x 1)))

    (iter 1))

(define (cons a b)
    (* (power 2 a) (power 3 b)))

(define (car z)
    (count-power z 2))

(define (cdr z)
    (count-power z 3))

(display (car (cons 2 5)))
(newline)
(display (cdr (cons 2 5)))
