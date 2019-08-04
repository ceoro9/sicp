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

; -----------------------------------------------------
; -----------------------------------------------------
; -----------------------------------------------------

(define (interleave s1 s2)
    (if (stream-null? s1)
        s2
        (cons-stream (stream-car s1)
                     (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
    (if (or (stream-null? s) (stream-null? t))
        the-empty-stream
        (cons-stream
            (list (stream-car s) (stream-car t))
            (interleave
                (stream-map (lambda (x) (list (stream-car s) x))
                            (stream-cdr t))
                (pairs (stream-cdr s) (stream-cdr t))))))

(define (triples s t u)
    (if (or (stream-null? s) (stream-null? t) (stream-null? u))
        the-empty-stream
        (cons-stream (list (stream-car s)
                           (stream-car t)
                           (stream-car u))
                     (interleave
                        ; pass first element, because it was already added on previous
                        ; step. This trick is necessary to have a delayed evaluation.
                        (stream-cdr (stream-map (lambda (x) (cons (stream-car s) x)) (pairs t u)))
                        (triples (stream-cdr s)
                                 (stream-cdr t)
                                 (stream-cdr u))))))

(define input-stream (cons-stream 1 (cons-stream 2 (cons-stream 3 the-empty-stream))))
(define result (triples input-stream input-stream input-stream))

(display-stream result)


