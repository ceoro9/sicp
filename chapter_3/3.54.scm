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

; ------------------------------------------------------------------
; ------------------------------------------------------------------
; ------------------------------------------------------------------

(define (mul-streams s1 s2)
    (stream-map * s1 s2))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
    (stream-map + s1 s2))

(define integers
    (cons-stream 1 (add-streams ones integers)))

(define factorials
    (cons-stream 1 (mul-streams integers factorials)))

(define (factorial n)
    (stream-ref factorials n))

(display (factorial 0))
(newline)
(display (factorial 5))
(newline)
(display (factorial 10))







