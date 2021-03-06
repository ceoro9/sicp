(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0))
          ((sum? exp) (make-sum (deriv (addend exp) var)
                                (deriv (augend exp) var)))
          ((product? exp)
            (make-sum (make-product (multiplier exp)
                                    (deriv (multiplicand exp) var))
                      (make-product (multiplicand exp)
                                    (deriv (multiplier exp) var))))
          ((exponentation? exp)
            (make-product (make-product (exponent exp)
                                        (make-exponentation (base exp)
                                                            (make-sum (exponent exp) -1)))
                          (deriv (base exp) var)))
          (else (error "unknow expression type"))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list a1 '+ a2))))

(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list m1 '* m2))))

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))
(define (augend s)
    (define (handle seq result)
        (cond ((and (pair? seq) (pair? (cdr seq)))
                  (handle (cdr (cdr seq))
                          (make-sum result (car seq))))
              ((and (pair? seq) (null? (cdr seq)))
                  (make-sum result (car seq)))
              (else result)))
    (handle (cdr (cdr s)) (make-sum 0 0)))


(define (product? x) (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

(define (pow a n)
    (cond ((= n 0) 1)
          (else (* a (pow a (- n 1))))))

(define (make-exponentation a n)
    (cond ((= n 1) a)
          ((= n 0) 1)
          ((and (number? a) (number? n)) (pow a n))
          (else (list a '^ n))))

(define (base s) (car s))
(define (exponent s) (caddr s))

(define (exponentation? x) (and (pair? x) (eq? (cadr x) '^)))

(display (deriv '(5 + 6 + 7) 'x))
