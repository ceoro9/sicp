(define (even? x) (= (remainder x 2) 0))

(define simpson-integral-number 40)

(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a) 
           (sum term (next a) next b))))

(define (integral f a b)
    (simpson-integral f a b simpson-integral-number))

(define (simpson-integral f a b n)
    (define h (/ (- b a) n))
    
    (define (term-val k)
        (cond ((OR (= k 0) (= k n)) (f (+ a (* k h))))
              ((even? k) (* 2 (f (+ a (* k h)))))
              (else (* 4 (f (+ a (* k h)))))))

    (define (inc x) (+ x 1))

    (* (/ h 3) (sum term-val 0 inc n)))


(define (cube x) (* x x x))


(display (integral cube 0 1))
