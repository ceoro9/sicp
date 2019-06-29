; A tower of types
;
;   complex
;      ^
;      |
;     real
;      ^
;      |
;   rational
;      ^
;      |
;   integer

(define types-list '(integer rational real complex))

(define (raise var type-result)
    
    (define (raising t-list result)
        (if (equal? (type-tag result) type-result)
            result
            (if (null? t-list)
                (error "Unable to raise types" (list (type-tag var) type-result))
                (raising (cdr t-list)
                         ((get 'raise
                               (list (type-tag result) 
                                     (car t-list)))
                            result)))))

    (define (find-initial-type t-list)
        (if (null? t-list)
            (error "Unable to raise types" (list (type-tag var) type-result))
            (if (equal? (car t-list) (type-tag var))
                (raising (cdr t-list) var)
                (handle (cdr t-list)))))

    (find-initial-type types-list))


(define (install-integer-package)
   
    (define (raise x) (make-rational x 1))

    (put 'raise '(integer rational) raise))

(define (install-rational-package)
    
    (define (raise x) (/ (numer x) (denom x)))

    (put 'raise '(rational real) raise))

(define (install-real-package)
    
    (define (raise x) (make-complex-from-real-image x 1))

    (put 'raise '(real complex) raise))
