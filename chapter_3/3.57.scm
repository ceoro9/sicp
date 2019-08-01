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

(define (add-streams s1 s2)
    (stream-map (lambda (x y) (+ x y)) s1 s2))

; -------------------------------------------------------------------------
; -------------------------------------------------------------------------
; -------------------------------------------------------------------------

(define fibs
  (cons-stream
   0
   (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))


; cdr-fibs:         1 1 2 3 5 8  ...
; fibs:             0 1 1 2 3 5  ...
; new-fibs: (0 1) + 1 2 3 5 8 13 ...


(stream-ref fibs 10)

; How many additions are performed when we compute the n th Fibonacci number?
;
; If memoization is provided - O(n), because when we need to compute A(n) Fibonacciy number
; A(n - 1) and A(n - 2) will be got from cache, because there were computed on previous steps.
; But if we don't have memorization, on every step we will need again and again computer the
; previous 2 values, that leads us to O(f^n) complexity, where f - golden ratio.



