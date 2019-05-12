; |--------------------------------|
; | Functions below do not contain |
; | validation of input params     |
; |--------------------------------|

(define (dot-product v w)
    (if (null? v)
        '()
        (append (list (* (car v) (car w)))
                (dot-product (cdr v) (cdr w)))))

(define (accumulate p start-value sequance)
    (define (iter c result)
        (if (null? c)
            result
            (iter (cdr c)
                  (p result (car c)))))
    (iter sequance start-value))

(define (handle g1 g2)
    (accumulate (lambda (a b) (+ a b))
                0
                (dot-product g1 g2)))

(define (matrix-*-vector m v)
    (if (null? m)
        '()
        (append (list (handle (car m) v))
                (matrix-*-vector (cdr m) v))))

(define (first-elements-of-sequances seq)
    (map (lambda (s) (car s)) seq))

(define (sequances-without-first-element seq)
    (map (lambda (s) (cdr s)) seq))

(define (accumulate-n p initial seq)
    (if (null? (car seq))
        '()
        (cons (accumulate p initial (first-elements-of-sequances seq))
              (accumulate-n p initial (sequances-without-first-element seq)))))

(define (inner-handle iv im)
    (if (null? (car im))
        '()
        (append (list (handle iv (first-elements-of-sequances im)))
                (inner-handle iv (sequances-without-first-element im)))))

(define (matrix-*-matrix m n)
    (if (null? m)
        '()
        (append (list (inner-handle (car m) n))
                (matrix-*-matrix (cdr m) n))))

(define (transpose m)
    (accumulate-n (lambda (result x) (append result (list x))) '() m))


(display (matrix-*-vector (list (list 1 2) (list 5 6)) (list 10 10)))
(newline)
(display (matrix-*-matrix (list (list 1 2) (list 1 2)) (list (list 1 2) (list 1 2))))
(newline)
(display (transpose (list (list 1 2) (list 3 4))))
