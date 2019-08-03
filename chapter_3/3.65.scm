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

(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
    (stream-map + s1 s2))

(define (partial-sums a)
    (define result
      (cons-stream (stream-car a)
                   (add-streams (stream-cdr a)
                                result)))
    result)

(define (stream-limit s tolerance)
    (define (iter v1 s)
        (if (> tolerance (abs (- v1 (stream-car s))))
            (stream-car s)
            (iter (stream-car s)
                  (stream-cdr s))))
    (iter (stream-car s) (stream-cdr s)))

(define (ln2 n)
    (cons-stream (/ 1.0 n)
                 (stream-map (lambda (x) (* x -1)) (ln2 (+ n 1)))))

(define ln2-stream (partial-sums (ln2 1)))

(display (stream-limit ln2-stream 0.1))

