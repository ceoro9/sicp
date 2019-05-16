(define (accumulate p start-value sequance)
    (define (iter c result)
        (if (null? c)
            result
            (iter (cdr c)
                  (p (car c) result))))
    (iter sequance start-value))

(define (enumerate-interval i j)
    (define (iter c result)
        (if (> c j)
            result
            (iter (+ c 1)
                  (append result (list c)))))
    (iter i (list)))

(define (get-pairs n)
    (accumulate append
                '()
                (map (lambda (i)
                    (map (lambda (j) (cons i j))
                         (enumerate-interval 1 (- i 1))))
                    (enumerate-interval 1 n))))

(define (flatmap p seq) (accumulate append '() (map p seq)))

(define (remove item s)
    (filter (lambda (x) (not (= item x))) s))

(define (permutations s)
    (if (null? s)
        (list '())
        (flatmap (lambda (x)
                    (map (lambda (p) (cons x p))
                         (permutations (remove x s))))
                 s)))

(display (permutations (list 1 2 3)))























