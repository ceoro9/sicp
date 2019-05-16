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

(define (ordered-triples-sum n sum) 
    (filter (lambda (seq) (= (accumulate + 0 seq) sum)) 
            (flatmap 
                (lambda (i) 
                (flatmap (lambda (j) 
                           (map (lambda (k) (list i j k)) 
                                  (enumerate-interval (+ j 1) n))) 
                           (enumerate-interval (+ i 1) n))) 
                 (enumerate-interval 1 n)))) 


(display (ordered-triples-sum 5 8))
