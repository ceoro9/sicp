(define (count-leaves x)
    (cond ((null? x) 0)
          ((not (pair? x)) 1)
          (else (+ (count-leaves (car x))
                   (count-leaves (cdr x))))))

(define (accumulate p start-value sequance)
    (define (iter c result)
        (if (null? c)
            result
            (iter (cdr c)
                  (p result (car c)))))
    (iter sequance start-value))

(define (acc-count-leaves t)
    (if (NOT (pair? t))
        1
        (accumulate (lambda (result x)
                        (if (pair? x)
                            (+
                                (acc-count-leaves (car x))
                                (acc-count-leaves (cdr x))
                                result)
                            (+ result 1)))
                    0
                    (map (lambda (x) 
                            (if (pair? x)
                                x
                                '()
                            )) 
                        t))))

(display (count-leaves (list 1 2 (list 1 (list 3 4)))))
(newline)
(display (acc-count-leaves (list 1 2 (list 1 (list 3 4)))))

(newline)
(newline)

(display (count-leaves (list 1 2 (list 5 6) (list 1 (list 3 4 5)))))
(newline)
(display (acc-count-leaves (list 1 2 (list 5 6) (list 1 (list 3 4 5)))))
