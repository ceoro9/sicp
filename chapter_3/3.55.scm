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

(define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin
          (proc (stream-car s))
          (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
    (stream-for-each display-line s))

(define (display-line s)
    (newline)
    (display s))

; ----------------------------------------------------------------------
; ----------------------------------------------------------------------
; ----------------------------------------------------------------------

(define (add-streams s1 s2)
    (stream-map + s1 s2))

(define (partial-sums a)
    (define result
      (cons-stream (stream-car a)
                   (add-streams (stream-cdr a)
                                result)))
    result)

(define result
  (partial-sums
    (cons-stream 1
      (cons-stream 2
        (cons-stream 10
          (cons-stream 20
            the-empty-stream))))))


(display-stream result)

