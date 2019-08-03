(define (force delayed-object) (delayed-object))

(define-syntax delay
  (syntax-rules ()
    ((_ exp) (lambda () exp))))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))
(define the-empty-stream '())
(define (stream-null? s) (eq? s the-empty-stream))

(define (stream-ref s n)
    (newline)
    (display (stream-car s))
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))

(define (scale-stream s factor)
    (stream-map (lambda (x) (* x factor)) s))

(define (add-streams s1 s2)
    (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (mul-series s1 s2)
    (cons-stream (* (stream-car s1)
                    (stream-car s2))
                 (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                              (mul-series (stream-cdr s1) s2))))

(define (invert-unit-series s)
    (define x
      (cons-stream 1
        (scale-stream
          (mul-series (stream-cdr s) x)
          -1)))
    x)

(define input (cons-stream 1 (cons-stream 2 (cons-stream 3 the-empty-stream))))
(define result (invert-unit-series input))

(define result-to-check (mul-series input result))

(stream-ref result-to-check 1) ; == 1

