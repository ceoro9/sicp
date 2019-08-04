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

(define (stream-ref s n)
    (newline)
    (display (stream-car s))
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

(define (add-streams s1 s2)
    (stream-map + s1 s2))

(define (scale-stream s factor)
    (stream-map (lambda (x) (* x factor)) s))

; -----------------------------------------------------------------
; -----------------------------------------------------------------
; -----------------------------------------------------------------

(define (integral integrand initial-value dt)
    (define int
      (cons-stream initial-value
                   (add-streams (scale-stream integrand dt) int)))
    int)

(define (RC R C dt)
    (lambda (s v0)
      (add-streams
        (integral (scale-stream s (/ 1 C)) v0 dt)
        (scale-stream s R))))

(define RC1 (RC 5 1 0.5))

(define ones (cons-stream 1 ones))

(stream-ref (RC1 ones 0) 10)

