(define (smooth f)
    (define dx 0.0001)
    
    (define (average a b c)
        (/ (+ a b c) 3))

    (lambda (x)
        (average 
            (f (- x dx)) 
            (f x) 
            (f (+ x dx)
        ))))

(define (repeated f n)
    (define (iter i result)
        (if (> i n)
            result
            (iter (+ i 1) (f result))))
    (lambda (x)
        (iter 2 (f x))))

(define (smooth-n f n) ((repeated smooth n) f))
(display ((smooth-n cos 10) 1.0))



