(define (make-accumulator initial-sum)
    (lambda (x)
        (begin
            (set! initial-sum (+ initial-sum x))
            initial-sum)))

(define A (make-accumulator 10))
(display (A 10))
(newline)
(display (A 20))

