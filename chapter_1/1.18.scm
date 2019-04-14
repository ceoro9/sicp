(define (double x) (* 2 x))

(define (halve x) (/ x 2))

(define (even? x) (= (remainder x 2) 0))

(define (iter-fast-mul a b)
    (fast-mul-iter a b 0))

(define (fast-mul-iter a b r)
    (cond ((< b 1) r)
          ((even? b) (fast-mul-iter (double a) (halve b) r))
          (else (fast-mul-iter a (- b 1) (+ a r)))))

(display (iter-fast-mul 10 15))
