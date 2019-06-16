(define (element-of-set? x set)
    (cond ((null? set) #f)
          ((equal? x (car set)) #t)
          (else (element-of-set? x (cdr set)))))

(define (addjoin-set x set) (cons x set))

(define (intersection-set set1 set2)
    (define (handle s1 s2)
        (cond ((or (null? s1) (null? s2)) '())
              ((element-of-set? (car s1) s2)
                  (cons (car s1) (handle (cdr s1) s2)))
              (else (handle (cdr s1) s2))))
    
    (define inner (handle set1 set2))  ; get items from first set, which presence in 2nd one
    (filter
        (lambda (x) (element-of-set? x inner))
        (union-set set1 set2)))

(define (union-set set1 set2) (append set1 set2))

(define set1 (list 1 2 3 4))
(display "set1 = ")
(display set1)

(newline)

(define set2 (list 1 2 5))
(display "set2 = ")
(display set2)

(newline)
(newline)

(display "Intersection: ")
(display (intersection-set set1 set2))

(newline)

(display "Union: ")
(display (union-set set1 set2))
