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

(define (flatmap p seq) (accumulate append '() (map p seq)))

(define (unique-pairs n)
   (flatmap (lambda (i)
                    (map (lambda (j) (cons i j))
                         (enumerate-interval (+ i 1) n)))
            (enumerate-interval 1 n)))


(display (unique-pairs 5))

