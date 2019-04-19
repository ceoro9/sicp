; recursion
(define (product f a next b)
    (if (> a b)
        1
        (* (f a) (product f (next a) next b))
    ))

; iteration
(define (product-iter f a next b)
    (define (iter c result)
        (if (> c b)
            result
            (iter (next c) (* (f c) result))))
    (iter a 1)
)

; calculate factorial
(define (val x) x)
(define (inc x) (+ x 1))
(display (product-iter val 1 inc 5)) 

; calculate Pi
(define (cal-pi n) 
    (define (next-val x) (+ x 2))
    (define (square x) (* x x))
    (define (get-val a) (/ (square(+ a 1)) (square a)))
    (define (even? x) (= (remainder x 2) 0))
    
    (/ (* 4 (* 2 (product-iter get-val 3 next-val n))) n))



(newline)
(display (cal-pi 27))

