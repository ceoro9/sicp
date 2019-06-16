(define (element-of-set? x set)
    (cond ((null? set) #f)
          ((equal? x (car set)) #t)
          (else (element-of-set? x (cdr set)))))

(define (union-set set1 set2)
    (if (null? set1)
        set2
        (if (element-of-set? (car set1) set2)
            (union-set (cdr set1) set2)
            (union-set (cdr set1)
                       (cons (car set1) set2)))))

(display (union-set (list 1 2 3) (list 4 5 1)))
