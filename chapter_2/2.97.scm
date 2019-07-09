; ---------------------------------------------------------------------------------
; Data-directed map

(define (create-data-direct-map) '())
(define data-direct-map (create-data-direct-map))
(define (make-data-direct-map-item operation type item) (list operation type item))
(define (select-operation dd-map-item) (car dd-map-item))
(define (select-type dd-map-item) (cadr dd-map-item))
(define (select-item dd-map-item) (caddr dd-map-item))

(define (get-from-dd-map dd-map operation type)
    (cond ((null? dd-map) #f)
          ((and (equal? (select-operation (car dd-map))
                      operation)
                (equal? (select-type (car dd-map))
                      type)) (select-item (car dd-map)))
           (else (get-from-dd-map (cdr dd-map) operation type))))

(define (put-to-dd-map dd-map operation type item)
    (cons (make-data-direct-map-item operation
                                     type
                                     item)
          dd-map))

(define (put operation type item)
    (define new-dd-map (put-to-dd-map data-direct-map operation type item))
    (set! data-direct-map new-dd-map))

(define (get operation type)
    (get-from-dd-map data-direct-map
                     operation
                     type))

; ---------------------------------------------------------------------------------
; Other packages

(define (install-scheme-number-package)
    
    (define (gcd-number a b)
        (define (handle n1 n2)
            (if (= n2 0)
                n1
                (handle n2 (remainder n1 n2))))
        (if (> a b)
            (handle a b)
            (handle b a)))
    
    (define (reduce a b)
        (let ((g (gcd-number a b)))
            (list
                (/ a g)
                (/ b g))))

    (put 'add '(scheme-number scheme-number)
        (lambda (x y) (+ x y)))

    (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (- x y)))

    (put 'mul '(scheme-number scheme-number)
        (lambda (x y) (* x y)))

    (put 'div '(scheme-number scheme-number)
        (lambda (x y) (/ x y)))
    
    (put 'gcd '(scheme-number scheme-number)
        (lambda (x y) (gcd-number x y)))

    (put 'reduce '(scheme-number scheme-number) reduce))

(define (install-rational-package)

    (define (tag x) (attach-tag 'rational x))

    (define (make-rat n d)
        (let ((g (reduce n d)))
            (cons (car g)
                  (cadr g))))
    
    (define (numer x) (car x))
    (define (denom x) (cdr x))

    (define (add-rat x y)
        (make-rat (add (mul (numer x) (denom y))
                       (mul (numer y) (denom x)))
                  (mul (denom x) (denom y))))
    (define (sub-rat x y)
        (make-rat (sub (mul (numer x) (denom y))
                       (mul (numer y) (denom x)))
                  (mul (denom x) (denom y))))

    (define (mul-rat x y)
        (make-rat (mul (numer x) (numer y))
                  (mul (denom x) (denom y))))

    (define (div-rat x y)
        (make-rat (mul (numer x) (denom y))
                  (mul (denom x) (numer y))))
    
    (put 'make '(rational)
        (lambda (x y) (tag (make-rat x y))))

    (put 'add '(rational rational)
        (lambda (x y) (tag (add-rat x y))))

    (put 'sub '(rational rational)
        (lambda (x y) (tag (sub-rat x y))))

    (put 'mul '(rational rational)
        (lambda (x y) (tag (mul-rat x y))))

    (put 'div '(rational rational)
        (lambda (x y) (tag (div-rat x y)))))

(define (make-rational d n)
    ((get 'make '(rational)) d n))

; ---------------------------------------------------------------------------------
; General functions

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

(define (apply-generic op . args)
    (let ((proc (get op (map type-tag args))))
        (if proc
            (apply proc (map contents args))
            (error "No such operations" (car args))))))

(define (same-variable? a b) (equal? a b))

(define (add a b) (apply-generic 'add a b))
(define (sub a b) (apply-generic 'sub a b))
(define (mul a b) (apply-generic 'mul a b))
(define (div a b) (apply-generic 'div a b))
(define (gcd a b) (apply-generic 'gcd a b))
(define (reduce a b) (apply-generic 'reduce a b))
; ---------------------------------------------------------------------------------
; Polynomial term (signle implementation)

(define (adjoin-term term term-list)
    (if (zero? (coeff term))
        term-list
        (cons term term-list)))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (cons order coeff))
(define (order term) (car term))
(define (coeff term) (cdr term))

(define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
              (let ((order-1 (order t1))
                    (coeff-1 (coeff t1))
                    (order-2 (order t2))
                    (coeff-2 (coeff t2)))
                  (cond ((= order-1 order-2)
                         (adjoin-term (make-term order-1
                                                 (+ coeff-1 coeff-2))
                                      (add-terms (rest-terms L1)
                                                 (rest-terms L2))))
                        ((> order-1 order-2)
                         (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                        (else (adjoin-term t2 (add-terms L1 (rest-terms L2))))))))))

(define (sub-terms L1 L2)
    (add-terms L1 (mul-terms (list (make-term 0 -1)) L2)))

(define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (* (coeff t1) (coeff t2))) ; use generic operation
           (mul-term-by-all-terms t1 (rest-terms L))))))

(define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (/ (coeff t1) (coeff t2))) ; use generic operation
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                    ; subtract-terms and mul-terms
                    ; should be implemented in term package,
                    ; take a look on 2.90 exercise.
                    ; Plese do it on ur own.
                    (div-terms (sub-terms 
                                  L1
                                  (mul-terms (list (make-term new-o new-c)) L2))
                               L2)))
                    (list
                        (adjoin-term (make-term new-o new-c)
                                     (car rest-of-result))
                        (cadr rest-of-result))))))))

(define accumulate-terms
  (lambda (op base func terms)
    (if (null? terms)
        base
        (op (func (first-term terms))
            (accumulate-terms op base func (rest-terms terms))))))

(define (quotient-terms T1 T2) (car (div-terms T1 T2)))
(define (remainder-terms T1 T2) (cadr (div-terms T1 T2)))
(define (gcd-terms T1 T2)
    (define (handle t1 t2)
        (if (empty-termlist? T2)
            T1
            (gcd-terms T2 (pseudoremainder-terms t1 t2))))
    
    (define (get-result)
        (cond ((empty-termlist? T1) (handle T2 T1))
              ((empty-termlist? T2) (handle T1 T2))
              ((> (order (first-term T1))
                (order (first-term T2)))
                    (handle T1 T2))
              (else (handle T2 T1))))
    
    (let ((result-terms (get-result)))
      (let ((common-divisor (accumulate-terms gcd 0 coeff result-terms)))
        (map
            (lambda (term)
                (make-term (order term)
                           (div (coeff term)
                                common-divisor)))
            result-terms))))

(define (reduce-terms n d)
    (define (reduce-terms-coeff terms)
        (let ((common-divisor (accumulate-terms gcd 0 coeff terms)))
            (map
                (lambda (term)
                    (make-term (order term)
                               (div (coeff term) common-divisor)))
                terms)))
    (let ((gcd-result (gcd-terms n d)))
      (let ((factor (expt (coeff (first-term gcd-result))
                          (+ 1 (- (max (order (first-term n))
                                       (order (first-term d)))
                                  (order (first-term gcd-result)))))))
        (let ((n-result (quotient-terms (mul-terms n (list (make-term 0 factor))) gcd-result))
              (d-result (quotient-terms (mul-terms d (list (make-term 0 factor))) gcd-result)))
            (list
                (reduce-terms-coeff n-result)
                (reduce-terms-coeff d-result))))))

(define (pseudoremainder-terms a b)
    (let ((factor (expt (coeff (first-term b))
                        (- (+ 1 (order (first-term a))) (order (first-term b))))))
      (cadr (div-terms
        (mul-terms
            a
            (list (make-term 0 factor)))
        b))))

; ---------------------------------------------------------------------------------
; Main operations on polynomials

(define (make-dense-polynomial var terms)
    ((get 'make-dense-polynomial '(polynomial)) var terms))

(define (make-sparse-polynomial var terms)
    ((get 'make-sparse-polynomial '(polynomial)) var terms))
 
(define (variable p) (apply-generic 'variable p))
(define (term-list p) (apply-generic 'term-list p))

; ---------------------------------------------------------------------------------
; Base polynomial package

(define (install-polynomial-package)
    
    (define (tag s) (attach-tag 'polynomial s))
    
    (define (make-dense-polynomial var terms)
        (tag ((get 'make '(dense-polynomial)) var terms)))

    (define (make-sparse-polynomial var terms)
        (tag ((get 'make '(sparse-polynomial)) var terms)))

    (define (term-list p) (apply-generic 'term-list p))

    (define (add-poly P1 P2)
        (if (same-variable? (variable P1) (variable P2))
            (make-dense-polynomial (variable P1)
                                   (mul-terms (term-list P1)
                                              (term-list P2)))
            (error "Polys not in same var")))

    (define (mul-poly P1 P2)
        (if (same-variable? (variable P1) (variable P2))
            (make-dense-polynomial (variable P1)
                                   (mul-terms (term-list P1)
                                              (term-list P2)))
            (error "Polys not in same var")))
    
    (define (sub-poly P1 P2)
        (if (same-variable? (variable P1) (variable P2))
            (make-dense-polynomial (variable P1)
                                   (sub-terms (term-list P1)
                                              (term-list P2)))
            (error "Polys not in same var")))

    (define (div-poly P1 P2) ; -> (list poly poly)
        (if (same-variable? (variable P1) (variable P2))
            (let ((result (div-terms (term-list P1)
                                     (term-list P2))))
            (list
                (make-dense-polynomial (variable P1) (car result))
                (make-dense-polynomial (variable P2) (cdr result))))
            (error "cannot divide polynomials with different variable")))
    
    (define (gcd-poly P1 P2)
       (if (same-variable? (variable P1) (variable P2))
           (make-dense-polynomial (variable P1)
                                  (gcd-terms (term-list P1)
                                             (term-list P2)))
           (error "cannot divide polynomials with different variable")))

    (define (reduce-poly P1 P2)
        (if (same-variable? (variable P1) (variable P2))
            (let ((result (reduce-terms (term-list P1) (term-list P2))))
                (list
                    (make-sparse-polynomial (variable P1) (car result))
                    (make-sparse-polynomial (variable P2) (cadr result))))
            (list P1 P2)))

    ; dependencies on others packages
    (install-sparse-polynomial-package)
    (install-dense-polynomial-package)
    
    (put 'make-dense-polynomial '(polynomial) make-dense-polynomial)
    (put 'make-sparse-polynomial '(polynomial) make-sparse-polynomial)
    (put 'term-list '(polynomial) term-list)
    (put 'add '(polynomial polynomial) add-poly)
    (put 'mul '(polynomial polynomial) mul-poly)
    (put 'sub '(polynomial polynomial) sub-poly)
    (put 'div '(polynomial polynomial) div-poly)
    (put 'gcd '(polynomial polynomial) gcd-poly)
    (put 'reduce '(polynomial polynomial) reduce-poly))

; ---------------------------------------------------------------------------------
; Sparse polynomial

(define (install-sparse-polynomial-package)

    (define (tag s) (attach-tag 'sparse-polynomial s))
    
    (define (make var terms) (tag (cons var terms)))
    (define (variable poly) (car poly)) 
    (define (term-list poly) (cdr poly))

    (define (adjoin-term term poly)
        (if (zero? (coeff term))
            sparse-poly
            (make (variable poly)
                  (cons term (term-list poly)))))
    
    (put 'make '(sparse-polynomial) make)
    (put 'variable '(sparse-polynomial) variable)
    (put 'term-list '(sparse-polynomial) term-list)
    (put 'adjoin-term '(term sparse-polynomial) adjoin-term))

; ---------------------------------------------------------------------------------
; Dense polynomial

(define (install-dense-polynomial-package)
    
    (define (tag s) (attach-tag 'dense-polynomial s))

    (define (make var terms) (tag (cons var terms)))
    (define (variable poly) (car poly))
    (define (term-list poly) (cdr poly))

    (define (adjoin-term term poly)
        (define (handle current-order terms)
            (if (= (order term) current-order)
                (cons (order term) terms)
                (handle (+ current-order 1) (cons 0 terms))))
        (if (zero? term)
            poly
            (make (variable poly)
                  (handle (length (term-list poly)
                          (term-list poly))))))

    (put 'make '(dense-polynomial) make)
    (put 'variable '(dense-polynomial) variable)
    (put 'term-list '(dense-polynomial) term-list)
    (put 'adjoin-term '(term dense-polynomial) adjoin-term))

; ---------------------------------------------------------------------------------

(install-rational-package)
(install-scheme-number-package)
(install-polynomial-package)

(define p1 (make-sparse-polynomial 'x (list (make-term 1 1) (make-term 0 1))))
(define p2 (make-sparse-polynomial 'x (list (make-term 3 1) (make-term 0 -1))))
(define p3 (make-sparse-polynomial 'x (list (make-term 1 1))))
(define p4 (make-sparse-polynomial 'x (list (make-term 2 1) (make-term 0 -1))))
(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))

(display (add rf1 rf2))

