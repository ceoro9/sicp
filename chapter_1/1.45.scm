(define (repeated f n)
    (define (iter i result)
        (if (> i n)
            result
            (iter (+ i 1) (f result))))
    (lambda (x)
        (iter 2 (f x))))

(define inflecity 0.00001)

(define (average a b)
    (/ (+ a b) 2.0))

(define (average-dump f)
    (lambda (x) (average x (f x))))

(define (abs x)
    (if (> x 0) 
        x
        (* -1 x)))

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) inflecity))
    (define (try guess)
        (let ((next (f guess))) 
            (if (close-enough? next guess) 
                next
                (try next))))
    (try first-guess))

(define (even? x)
    (= (remainder x 2) 0)) 

(define (square x)
    (* x x))

(define (power b n)
    (cond ((= n 0) 1)
          ((even? n) (square (power b (/ n 2))))
          (else (* b (power b (- n 1))))))

(define (average x y)
    (/ (+ x y) 2))


; calculate log2
(define (get-dump-count n) 
    (define (iter p r) 
        (if (> r n) 
            (- p 1) 
            (iter (+ p 1) (* r 2))))                               
    (iter 1 2)) 


(define (sqrt x n) 
   (fixed-point ((repeated average-dump (get-dump-count n)) 
                    (lambda (y) (/ x (power y (- n 1))))) 
                                    1.0)) 


(display (sqrt 81 4))
(newline)
(display (sqrt 32 5))
