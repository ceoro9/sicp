(define (install-polynomial-term-package)
     
    (define (tag s) (attach-tag 'term s))

    ; Term implementation is common for sparse and dense polynomials
    (define (make order coeff) (tag 'term (cons order coeff)))
    (define (order term) (car term))
    (define (coeff term) (cdr term))

    (put 'make '(term) make)
    (put 'order '(term) order)
    (put 'coeff '(term) coeff))

(define (install-polynomial-package)
    
    (define (tag s) (attach-tag 'polynomial s))

    (define (make-sparse-polynomial var terms)
        (tag (('get 'make '(sparse-polynomial)) var terms)))

    (define (make-dense-polynomial var terms)
        (tag (('get 'make '(dense-polynomial)) var terms)))
    
    (define (adjoin-term term poly)
        (tag (apply-generic 'adjoin-term term poly)))

    (put 'make-sparse '(polynomial) make-sparse-polynomial)
    (put 'make-dense '(polynomial) make-dense-polynonial)
    
    ; dependecy on term package
    (install-polynomial-term-package))

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
    (put 'adjoin-term '(term sparse-polynomial) adjoin-term))


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
    (put 'adjoin-term '(term dense-polynomial) adjoin-term))

(define (make-term order coeff)
    (('get 'make '(term)) order coeff))

(define (order term) (apply-generic 'order term))
(define (coeff term) (apply-generic 'coeff term))

(define (make-sparse-polynomial var terms)
    ((get 'make-sparse '(polynomial)) var terms))

(define (make-dense-polynomial var coeffs)
    (('get 'make-dense '(polynomial)) var coeffs))

(define (adjoin-term term poly)
    (apply-generic 'adjoin-term term poly))
