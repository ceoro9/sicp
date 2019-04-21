(define (cont-frac n d k)
    (define (iter i result)
        (if (= i 0)
            result
            (iter (- i 1) (/ (n i) (+ (d i) result)))))
    (iter k 0))

(define (Euler-d i)
    (cond ((= i 1) 1)
          ((= i 2) 2)
          ((= (remainder (- i 2) 3) 0) (* (+ (/ (- i 2) 3) 1) 2))
          (else 1)))

(display (cont-frac (lambda (i) 1.0) Euler-d 10))

