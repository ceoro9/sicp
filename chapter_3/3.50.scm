(define (memo-proc proc)
    (let ((already-run? false) (result false))
      (lambda ()
          (if (not already-run?)
              (begin (set! result (proc))
                     (set! already-run? true)
                     result)
              result))))

(define (delay expn) (memo-proc (lambda () expn)))
(define (force delayed-object) (delayed-object))

(define (cons-stream a b) (cons a (delay b)))
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))
(define the-empty-stream '())
(define (stream-null? s) (eq? s the-empty-stream))

(define (stream-enumerate-interval low high)
    (if (> low high)
        the-empty-stream
        (cons-stream low
                     (stream-enumerate-interval (+ low 1) high))))

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

; -------------------------------------------------------------------------
; -------------------------------------------------------------------------
; -------------------------------------------------------------------------

(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))

(define result
  (stream-map
    +
    (stream-enumerate-interval 10 20)
    (stream-enumerate-interval 50 60)
    (stream-enumerate-interval 80 90)))

(display-stream result)


