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

(define (stream-scale s factor)
    (stream-map (lambda (x) (* x factor)) s))

; -----------------------------------------------------------------
; -----------------------------------------------------------------
; -----------------------------------------------------------------

(define (integral delayed-integrand initial-value dt)
    (cons-stream
      initial-value
      (let ((integrand (force delayed-integrand)))
        (if (stream-null? integrand)
            the-empty-stream
            (integral (delay (stream-cdr integrand))
                      (+ (* dt (stream-car integrand))
                          initial-value)
                      dt)))))

(define (RLC R L C dt)
    (lambda (vc0 il0)
      (define vc
        (integral
          (delay (stream-scale il (/ -1 C)))
           vc0
           dt))
      (define il
        (integral
          (delay
            (add-streams
              (stream-scale vc (/ 1 L))
              (stream-scale il (/ (* R -1) L))))
           il0
           dt))
      (stream-map cons vc il)))

(define rlc-example (RLC 1 1 0.2 0.1))
(define result (rlc-example 10 0))

(stream-ref result 10)

