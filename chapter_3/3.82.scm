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
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

; ------------------------------------------------------
; ------------------------------------------------------
; ------------------------------------------------------

(define (make-2d-point x y) (cons x y))
(define (point-x p) (car p))
(define (point-y p) (cdr p))

(define (random-in-range low high)
    (let ((range (- high low)))
      (+ low (random range))))

(define (square x) (* x x))

(define (map-successive-pairs f s)
    (cons-stream
      (f (stream-car s) (stream-car (stream-cdr s)))
      (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define (monte-carlo experiment-stream passed failed)
    
    (define (next passed failed)
        (cons-stream
          (/ passed (+ passed failed))
          (monte-carlo
            (stream-cdr experiment-stream) passed failed)))
    
    (if (stream-car experiment-stream)
        (next (+ passed 1) failed)
        (next passed (+ failed 1))))

(define (estimate-integral P x1 y1 x2 y2)
    (define experiments
      (cons-stream
        ; delayed evaluation in order to have
        ; different random number in stream
        (delay (make-2d-point (random-in-range x1 x2)
                              (random-in-range y1 y2)))
        experiments))
    (monte-carlo
      (stream-map
        ; here's we evaluate delayed expression to get a random
        (lambda (args) (P (force args)))
        experiments)
      0 0))

(define (point-in-circle? c-x c-y r)
    (lambda (p)
        (<= (+ (square (- (point-x p) c-x))
               (square (- (point-y p) c-y)))
            (square r))))

(define (estimate-pi trials)
    (let ((x1 2)
          (y1 4)
          (x2 8)
          (y2 10)
          (c-x 5)
          (c-y 7)
          (R 3))
      (let ((rectangle-area
            (* (abs (- x1 x2))
               (abs (- y1 y2)))))
        (let ((circle-area
              (* rectangle-area
                 (stream-ref
                   (estimate-integral
                     (point-in-circle? c-x c-y R)
                      x1 y1 x2 y2)
                   trials))))
          (exact->inexact (/ circle-area (square R)))))))

(display (estimate-pi 100000))

