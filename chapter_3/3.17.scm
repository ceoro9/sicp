(define (same-pairs? p1 p2)
    (let ((tmp1 (car p1))
          (answer '()))
      (set-car! p1 'wow)
      (if (eq? (car p2) 'wow)
          (set! answer #t)
          (set! answer #f))
      (set-car! p1 tmp1)
       answer))

(define (count-pairs x)
    (let ((processed-pairs '()))

      (define (processed? p)
          (define (iterate c pairs)
              (cond ((null? pairs) #f)
                    ((same-pairs? c (car pairs)) #t)
                    (else (iterate c (cdr pairs)))))
          (iterate p processed-pairs))

      (define (iterate p)
          (if (or (not (pair? p))
                  (processed? p))
              0
              (begin
                (set! processed-pairs (cons p processed-pairs))
                (+ 1
                  (iterate (car p))
                  (iterate (cdr p)))))) 
      (iterate x)))

(display (count-pairs (cons 1 2)))
(newline)

(define x '(foo))
(define y (cons x x))
(define str1 '(foo bar baz))
(define str3 (cons y y))
(define str2 (list y)) 

(display (count-pairs str1)) 
(newline)
(display (count-pairs str2)) 
(newline)
(display (count-pairs str3)) 
(newline)
