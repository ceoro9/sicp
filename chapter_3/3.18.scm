(define (same-pairs? p1 p2)
    (let ((tmp1 (car p1))
          (answer '()))
      (set-car! p1 'wow)
      (if (eq? (car p2) 'wow)
          (set! answer #t)
          (set! answer #f))
      (set-car! p1 tmp1)
       answer))

(define (cycle? l)
    (let ((processed-elements '()))

        (define (processed? el)
            (define (iter s)
                (cond ((null? s) #f)
                      ((same-pairs? (car s) el) #t)
                      (else (iter (cdr s)))))
            (iter processed-elements))

        (define (iter e)
            (cond ((null? e) #f)
                  ((processed? e) #t)
                  (else
                    (begin
                      (set! processed-elements (cons e processed-elements))
                      (iter (cdr e))))))
        (iter l)))

(define el (list 1 2 3))
(set-cdr! (cdr el) el)

(display (cycle? el))
(newline)

(display (cycle? (list 1 2 3)))
(newline)

