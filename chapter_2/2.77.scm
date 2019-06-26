(define (attach-tag tag contents)
    (cons tag contents)) 

(define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (error "Bad tagged datum: " datum)))

(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (error "Bad tagged datum: " datum)))

(define (apply-generic operation . args)
    (let ((types (map type-tag args)))
        (let ((proc (get operations types)))
            (if (proc)
                (apply proc (map contents args))
                (error "No such procedure")))))

(define (real-part x) (apply-generic 'real-part x))
(define (imag-part x) (apply-generic 'imag-part x))
(define (magnitude x) (apply-generic 'magnitude x))
(define (angle x) (apply-generic 'angle x))


(define (install-complex-package)
    ; ....
    ; real-part, imag-part, magnitude and angle
    ; are GENERIC procedeures !!!
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle))





; Why this works?
; Because we step by step based on tag dispatch object to proper procudere.

; Tracing:
 
(define z (make-complex-from-real-imag 3 4)) ; z = ('complex ('rectangular (3 4)))
(magnitude z)

; 1) apply-generic on complex number. Generic procudure will be dispatched.
((get 'magnitude '(complex)) (contents z)) ; (contents z) = ('rectangular (3 4))

; 2) apply-generic on rectangular representation of complex number.
;    Specic(not generic) procudere will be dispatched,
;    because this is the last "level" of object, only real content is left, no more tags.
((get 'magnitude '(rectangular)) (contents z)) ; (contents z) = (3 4)

