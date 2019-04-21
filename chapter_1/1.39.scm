(define (cont-frac n d k)
    (define (iter i result)
        (if (= i 0)
            result
            (iter (- i 1) (/ (n i) (+ (d i) result)))))
    (iter k 0))

(define (tan-cf x)
    (define (Lamber-n i)
        (if (= i 1) 
            x
            (* -1 (* x x))))

    (define (Lamber-d i)
        (- (* i 2) 1))

    (cont-frac Lamber-n Lamber-d 1000))

(display (tan-cf 0.785398)) ; ~ 1
