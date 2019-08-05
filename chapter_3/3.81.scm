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

; ---------------------------------------------------------------
; ---------------------------------------------------------------
; ---------------------------------------------------------------

; For simplicity
(define (rand-update x) (+ x 1))

(define (make-request type payload) (cons type payload))
(define (request-type request) (car request))
(define (request-payload request) (cdr request))

(define (random-numbers-generator random-init requests)
    (if (empty-stream? requests)
        the-empty-stream
        (let ((r (stream-car requests)))
          (let ((r-type (request-type r))
                (r-payload (request-payload r)))
            (cond ((eq? r-type 'generate)
                   (cons-stream
                     random-init
                     (random-numbers-generator
                       (rand-update random-init)
                       (stream-cdr requests))))
                  ((eq? r-type 'reset)
                   (cons-stream
                     r-payload
                     (random-numbers-generator
                       (rand-update r-payload)
                       (stream-cdr requests))))
                  (else (error "UNKNOWN request type: " (list r-type))))))))

(define random-numbers
  (random-numbers-generator
    1
    (cons-stream
      (make-request 'generate '())
      (cons-stream
        (make-request 'generate '())
        (cons-stream
          (make-request 'generate '())
          (cons-stream
            (make-request 'reset 1)
            (cons-stream
              (make-request 'generate '())
              the-empty-stream)))))))

(display-stream random-numbers)

