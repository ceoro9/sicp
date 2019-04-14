(define (max a b)
    (cond ((< a b) b)
    (else a)))

(define (min a b)
    (cond ((= (max a b) a) b)
    (else a)))

(define (smallest-divisor-handle a b)
    (cond ((= b 0) a)
    (else (smallest-divisor-handle b (remainder a b)))))

(define (smallest-divisor a b)
    (smallest-divisor-handle (min a b) (max a b)))

(display (smallest-divisor 199 (smallest-divisor 1999 19999)))


