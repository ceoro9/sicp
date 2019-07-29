(define (memo-proc proc)
    (let ((already-run? false) (result false))
      (lambda ()
          (if (not already-run?)
              (begin (set! result (proc))
                     (set! already-run? true)
                     result)
              result))))

(define (delay expn) (memo-proc (lambda () expn)))
(define (force delayed-object) (delayed-object))

(define (cons-stream a b) (cons a (delay b)))
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))
(define the-empty-stream '())
(define (stream-null? s) (eq? s the-empty-stream))

(define (stream-enumerate-interval low high)
    (if (> low high)
        the-empty-stream
        (cons-stream low
                     (stream-enumerate-interval (+ low 1) high))))

(define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin
          (proc (stream-car s))
          (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
    (stream-for-each display-line s))

(define (display-line x)
    (newline)
    (display x))

(define (stream-map proc s)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream (proc (stream-car s))
                     (stream-map proc (stream-cdr s)))))

(define (stream-filter pred s)
    (cond ((stream-null? s) the-empty-stream)
          ((pred (stream-car s))
           (cons-stream (stream-car s)
                        (stream-filter pred (stream-cdr s))))
          (else (stream-filter pred (stream-cdr s)))))

(define (stream-accumulate op base s)
    (if (stream-null? s)
        base
        (op (stream-car s)
            (stream-accumulate
              op
              base
              (stream-cdr s)))))

(define s (stream-filter
            (lambda (x) (= (remainder x 2) 0))
            (stream-enumerate-interval 10 20)))

(display-stream s)

(newline)
(newline)

(display "SUM = ")
(display (stream-accumulate + 0 s))





