; ------------------------------------------------------------------

(define (=number? exp num) (and (number? exp) (= exp num)))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

; ------------------------------------------------------------------
(define (create-data-direct-map) '())

(define (make-data-direct-map-item operation type item) (list operation type item))
(define (select-operation dd-map-item) (car dd-map-item))
(define (select-type dd-map-item) (cadr dd-map-item))
(define (select-item dd-map-item) (caddr dd-map-item))

(define data-direct-map (create-data-direct-map))

(define (get-from-dd-map dd-map operation type)
    (cond ((null? dd-map) #f)
          ((and (eq? (select-operation (car dd-map))
                      operation)
                (eq? (select-type (car dd-map))
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

; ------------------------------------------------------------------

(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0))
          (else ((get 'deriv (operator exp)) (operands exp) var))))

; ------------------------------------------------------------------

(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
           (else (list '+ a1 a2))))

; ------------------------------------------------------------------

(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
           (else (list '* m1 m2))))

; ------------------------------------------------------------------

(define (install-sum-deriv)
    
    (define (handle-exp operands var)
        (make-sum (deriv (car operands) var)
                  (deriv (cadr operands) var)))
    
    (put 'deriv '+ handle-exp))

(define (install-product-deriv)
    
    (define (handle-exp operands var)
        (make-sum (make-product (car operands)
                                (deriv (cadr operands) var))
                  (make-product (cadr operands)
                                (deriv (car operands) var))))

    (put 'deriv '* handle-exp))

; ------------------------------------------------------------------

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(install-sum-deriv)
(install-product-deriv)


(display (deriv '(+ (* 5 (+ 3 x)) (* 4 x)) 'x))








