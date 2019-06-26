(define (attach-tag tag contents)
    (if (or (number? contents) (symbol? contents))
        contents
        (cons tag contents)))
     
(define (type-tag datum)
    (cond ((number? datum) 'scheme-number)
          ((symbol? datum) 'scheme-symbol)
          ((pair? datum) (car datum))
          (else "Bad tagged datum: " datum)))

(define (contents datum)
    (cond ((or (number? datum) (symbol? datum)) datum)
          ((pair? datum) (cdr datum))
          (else "Bad tagged datum: " datum)))

(define (install-scheme-number-package)
    (put 'add '(scheme-number scheme-number)
        (lambda (x y) (+ x y)))

    (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (- x y)))

    (put 'mul '(scheme-number scheme-number)
        (lambda (x y) (* x y)))

    (put 'div '(scheme-number scheme-number)
        (lambda (x y) (/ x y)))
    
    (put 'make '(scheme-number) (lambda (x) x))

    'done)


