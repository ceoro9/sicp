(define (cont-frac-rec n d k)
    (define (iter i)
        (if (= i k)
            (/ (n i) (d i))  
            (/ (n i) (+ (d i) (iter (+ i 1))))))
    (iter 1))

(define (cont-frac-iter n d k)
    (define (iter i result)
        (if (= i 0)
            result
            (iter (- i 1) (/ (n i) (+ (d i) result)))))
    (iter k 0))


(display (cont-frac-rec (lambda (i) 1.0) (lambda (i) 1.0) 10))
(newline)
(display (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 10))

