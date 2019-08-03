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

; A)
(define (integrate-series s)
    (stream-map
      (lambda (a i) (* (/ 1 i) a))
      s
      integers))

(define result (integrate-series (cons-stream 5 (cons-stream 4 (cons-stream 3 the-empty-stream)))))

(stream-ref result 2)

(newline)
(newline)
(newline)

; B)
; (sin x)' = cos x
; (cos x)' = - sin x 

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))


(newline)
(display "SIN: ")
(stream-ref sine-series 5)

(newline)
(newline)

(display "COS: ")
(stream-ref cosine-series 5)

