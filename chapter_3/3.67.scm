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

(define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin
          (proc (stream-car s))
          (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
    (stream-for-each display-line s))

(define (display-line x)
    (newline)
    (display x))

(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
    (stream-map + s1 s2))

; -----------------------------------------------------------
; -----------------------------------------------------------
; -----------------------------------------------------------

; (1, 1) (1, 2) (1, 3) ... (1, N)
; (2, 1) (2, 2) (2, 3) ... (2, N)
; (3, 1) (3, 2) (3, 3) ... (3, N)
; ...
; (N, 1)               ... (N, N)

(define (interleave s1 s2)
    (if (stream-null? s1)
        s2
        (cons-stream (stream-car s1)
                     (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream
            (list (stream-car s) (stream-car t))
            (interleave
                (stream-map (lambda (x) (list (stream-car s) x))
                            (stream-cdr t))
                (pairs (stream-cdr s) t)))))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (limit-stream s n)
    (if (= n 0)
        the-empty-stream
        (cons-stream
          (stream-car s)
          (limit-stream
            (stream-cdr s)
            (- n 1)))))

(define result
    (pairs (limit-stream integers 3)
           (limit-stream integers 3)))

(display-stream result)

