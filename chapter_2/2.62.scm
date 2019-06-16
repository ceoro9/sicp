(define (union-set set1 set2)
    (define (handle s1 s2)
        (cond ((and (null? s1) (null? s2)) '())
              ((null? s1) s2)
              ((null? s2) s1)
              ((< (car s1) (car s2))
                  (cons (car s1)
                        (handle (cdr s1) s2)))
              ((> (car s1) (car s2))
                  (cons (car s2)
                        (handle s1 (cdr s2))))
              ; current elements are equal
              (else (cons (car s2)
                          (handle (cdr s1)
                                  (cdr s2))))))
    (handle set1 set2))


(display (union-set (list 1 2 5) (list 3 4 20)))


 
