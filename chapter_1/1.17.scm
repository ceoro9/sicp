(define (square x) (* x x))

(define (even? n) (= (remainder n 2) 0))

(define (double x) (* 2 x))

(define (halve x) (/ x 2))

(define (iter-fast-mul a b)
    (cond ((= b 0) 0)
          ((= b 1) a)
          ((even? b) (double (iter-fast-mul a (halve b))))
          (else (+ a (double (iter-fast-mul a (halve (- b 1))))))
    )
)

(display (iter-fast-mul 15555))
