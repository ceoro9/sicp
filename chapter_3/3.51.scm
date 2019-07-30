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

(define (display-line x)
    (newline)
    (display x))

; -------------------------------------------------------------------------
; -------------------------------------------------------------------------
; -------------------------------------------------------------------------


(define (show x)
    (display-line x)
    x)


; With stream implementation described in book, (10, 9 ... 0) should be printed
; because of eager evaluation, which acually is the reason why this implementation
; is wrong since it stores all values in memory, but does not generate them on fly.
; To fix this issue macroinstruction(macro) should be introduced, but in book they are
; only described in the next chapter. All this situation introduces misunderstanding
; about the real internal work of streams. That really sucks.

; 0
(define x
  (stream-map show
             (stream-enumerate-interval 0 10)))

(newline)
(newline)

(stream-ref x 5) ; 1 ... 5
(newline)
(stream-ref x 7) ; 1 ... 7

