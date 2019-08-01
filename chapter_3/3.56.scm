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

(define (stream-map proc s)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream (proc (stream-car s))
                     (stream-map proc (stream-cdr s)))))

(define (scale-stream s factor)
    (stream-map (lambda (x) (* x factor)) s))

; -------------------------------------------------------------------------
; -------------------------------------------------------------------------
; -------------------------------------------------------------------------

(define (merge s1 s2)
    (cond ((stream-null? s1) s2)
          ((stream-null? s2) s1)
          (else
            (let ((s1car (stream-car s1))
                  (s2car (stream-car s2)))
              (cond ((< s1car s2car)
                     (cons-stream
                      s1car
                      (merge (stream-cdr s1) s2)))
                    ((> s1car s2car)
                     (cons-stream
                      s2car
                      (merge s1 (stream-cdr s2))))
                    (else
                      (cons-stream
                       s1car
                       (merge (stream-cdr s1)
                              (stream-cdr s2)))))))))

(define S
  (cons-stream 1
    (merge (scale-stream S 2)
      (merge (scale-stream S 3)
             (scale-stream S 5)))))

(stream-ref S 10)

