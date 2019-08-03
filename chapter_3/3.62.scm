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

(define (integrate-series s)
    (stream-map
      (lambda (a i) (* (/ 1 i) a))
      s
      integers))

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

(define (div-series num-s dem-s)
    (if (= (stream-car dem-s) 0)
        (error "DIVISION ON ZERO ERROR!!! Constant term is zero.")
        (mul-series num-s (invert-unit-series dem-s))))
    
(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

; 0, 1, 0, 1/3, 0, 2/15, 0, 17/315 ...
(define tane-series (div-series sine-series cosine-series))

(stream-ref tane-series 7)

