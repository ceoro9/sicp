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

(define (stream-map proc s)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream (proc (stream-car s))
                     (stream-map proc (stream-cdr s)))))

(define (average a b) (/ (+ a b) 2))

(define (sqrt-improve guess x)
    (average guess (/ x guess)))

(define (sqrt-stream x)
    (define guesses
      (cons-stream
       1.0
       (stream-map (lambda (guess) (sqrt-improve guess x))
                   guesses)))
    guesses)

; works only for infinite streams
(define (stream-limit s tolerance)
    (define (iter v1 s)
        (if (> tolerance (abs (- v1 (stream-car s))))
            (stream-car s)
            (iter (stream-car s)
                  (stream-cdr s))))
    (iter (stream-car s) (stream-cdr s)))

(define (sqrt x tolerance)
    (stream-limit (sqrt-stream x) tolerance))

(display (sqrt 9 0.001))

