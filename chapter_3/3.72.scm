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

(define (stream-filter pred s)
    (cond ((stream-null? s) the-empty-stream)
          ((pred (stream-car s))
           (cons-stream (stream-car s)
                        (stream-filter pred (stream-cdr s))))
          (else (stream-filter pred (stream-cdr s)))))

(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
    (stream-map + s1 s2))

(define (scale-stream s factor)
    (stream-map (lambda (x) (* x factor)) s))

; -------------------------------------------------------------------------
; -------------------------------------------------------------------------
; -------------------------------------------------------------------------

(define (merge-weighted s1 s2 weight)
    (cond ((stream-null? s1) s2)
          ((stream-null? s2) s1)
          (else
            (let ((s1-weight (weight (stream-car s1)))
                  (s2-weight (weight (stream-car s2))))
              (if (< s1-weight s2-weight)
                  (cons-stream
                      (stream-car s1)
                      (merge-weighted (stream-cdr s1) s2 weight))
                  (cons-stream
                      (stream-car s2)
                      (merge-weighted s1 (stream-cdr s2) weight)))))))

(define (interleave s1 s2)
    (if (stream-null? s1)
        s2
        (cons-stream (stream-car s1)
                     (interleave s2 (stream-cdr s1)))))

(define (weighted-pairs s t weight)
    (if (or (stream-null? s) (stream-null? t))
        the-empty-stream
        (cons-stream
            (list (stream-car s) (stream-car t))
            (merge-weighted
                (stream-map (lambda (x) (list (stream-car s) x))
                            (stream-cdr t))
                (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
                weight))))
            
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (squre x) (* x x))

(define (search-ceoro9-numbers)
    
    (define (weight x)
      (let ((i (car x))
            (j (cadr x)))
        (+ (square i)
           (square j))))

    (define pairs-stream (weighted-pairs integers integers weight))
    
    (define (find-numbers s)
        (let ((first-weight (weight (stream-car s)))
              (second-weight (weight (stream-car (stream-cdr s))))
              (third-weight (weight (stream-car (stream-cdr (stream-cdr s))))))
          (if (= first-weight second-weight third-weight)
              (cons-stream first-weight
                           (find-numbers
                             (stream-filter
                               (lambda (x) (not (= (weight x) first-weight)))
                               (stream-cdr s))))
              (find-numbers (stream-cdr s)))))

    (find-numbers pairs-stream))

(define ceoro9-numbers (search-ceoro9-numbers))

(stream-ref ceoro9-numbers 5)

