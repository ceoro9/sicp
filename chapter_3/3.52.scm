(define (force delayed-object) (delayed-object))

; WTF??????????????????????????
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

(define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream (proc (stream-car s))
                     (stream-map proc (stream-cdr s)))))

(define (display-stream s)
    (stream-for-each display-line s))

(define (stream-filter pred s)
    (cond ((stream-null? s) the-empty-stream)
          ((pred (stream-car s))
           (cons-stream (stream-car s)
                        (stream-filter pred (stream-cdr s))))
          (else (stream-filter pred (stream-cdr s)))))

(define (display-line x)
    (newline)
    (display x))

; -------------------------------------------------------------------------
; -------------------------------------------------------------------------
; -------------------------------------------------------------------------
; !!! No memoization !!!

(define sum 0)
; FUCK U
(define (accum x) (set! sum (+ x sum)) sum)

; 1 STEP
; #############################

(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
; stream is created and the first element(1) is loaded
; sum = 1

; 2 STEP
; #############################

(define y (stream-filter even? seq))
; loads 2, 1 + 2 = 3, 3 is the next element
; loads 3, 3 + 3 = 6, 6 is the next element 
; 6 % 2 = 0 -> STOP!
; sum = 6

; 3 STEP
; #############################

(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))
; 1 was loaded on the first step
; loads 2: 2 + 6 = 8
; loads 3: 3 + 8 = 11
; loads 4: 11 + 4 = 15
; 15 % 5 = 0 -> STOP!
; sum = 15

; 4 STEP
; #############################

; Starting with 4, because 3 was last processed on 2 STEP 
; loads 4: 15 + 4 = 21, 21 is the next element
; 4 % 2 = 0, counter += 1
; loads 5: 21 + 5 = 26, 26 is the next element
; ....
; loads 16: 146 + 16 = 162 is the next element
; 16 % 2 = 0, counter += 1
; counter = 7 -> STOP!
; sum = 162
(stream-ref y 7) 

; 5 STEP
; #############################

; Starting with 5, because 4 was last processed on 3 STEP
; loads 5: 162 + 15 = 167
; loads 6: 167 + 6 = 173
; ...
; loads 20: 342 + 40 = 362
; sum = 362
(display-stream z)

; #############################

