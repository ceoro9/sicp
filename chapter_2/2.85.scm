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


(define (droppable? var type)
    (let ((equal?-proc (get 'equal (list type type)))
          (projected-var (get 'project (list (type-tag var) type))))
        (equal?-proc var (raise projected-var (type-tag var)))))

(define (drop var type-result)
    
    (define (dropping t-list result)
        (if (equal? (type-tag result) type-result)
            result
            (if (null? t-list)
                (error "Unable to drop types: " (list (type-tag var) type-result))
                (if (droppable? result (car t-list))
                    (dropping (cdr t-list)
                              ((get 'project (list (type-tag result) (car t-list))) result))
                    (error "Unable to drop types: " (list (type-tag var) type-result))))))

    (define (find-initial-type t-list)
        (if (null? t-list)
            (error "Unable to drop types: " (list (type-tag var) type-result))
            (if (equal? (car t-list) (type-tag var))
                (dropping (cdr t-list) var)
                (handle (cdr t-list)))))

    (find-initial-type (reverse types-list)))

(define (apply-generic op . args)
    
    (define (handle t-list result)
        (if (null? t-list)
            result
            (if (droppable? result (car t-list))
                (handle (cdr t-list) (drop result (car t-list)))
                result)))

    (define (simplify-answer answer t-list)
        (if (null? t-list)
            (error "No such type in tree: " (type-tag answer))
            (if (equal? (type-tag answer) (car t-list))
                (handle (cdr t-list) t-list)
                (simplify-answer answer (cdr t-list))))
    
    (simplify-answer
        ; call apply-generic from 2.83 task
        (apply handle-generic (append (list op) args))
        (reverse types-list)))

(define (install-rational-package)
        
     ; keep in mind that integer number is implemented as built-in scheme type
    (define (project x) (floor (/ (numer x) (denom x))))

    (put 'project '(rational integer) project))

(define (install-real-package)
    
    (define (project x)
        (let ((integer-part (floor x))
              (fractional-part (- x (floor x))
              (numerals-count (numerals-after-point-count x))) ; TODO
            (make-rational (+ (* integer-part (pow 10 numerals-count))
                              (* fractional-part (pow 10 numerals-count)))
                           (pow 10 numerals-count)))))

    (put 'project '(real rational) project))

(define (install-complex-package)
    
    ; keep in mind that real number is implemented as built-in scheme type
    (define (project x) (real-part x))
    
    (put 'project '(complex real) project))
