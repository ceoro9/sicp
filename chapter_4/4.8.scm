(define (eval exp env)
    ; ...
    ((let? exp) (eval (let->combination exp) env))
    ; ...
)

(define (make-let inits body) (list 'let inits body))
(define (let? expr) (tagged-list? expr 'let)) 
(define (let-inits expr)
    (if (named-let? expr)
        (caddr expr)
        (cadr expr)))
(define (let-vars expr) (map car (let-inits expr))) 
(define (let-values expr) (map cadr (let-inits expr))) 
(define (let-body expr)
    (if (named-let? expr)
        (cadddr expr)
        (caddr expr)))
(define (let-var-declr expr)
    (if (not (named-let?))
        (error "Ordinary let does not have var declaration")
        (cadr expr)))
(define (named-let? expr) (symbol? (cadr expr)))

(define (let->combination exp)
    (if (named-let? exp)
      ; TODO: make definition consturctor
      ; TODO: make begin constructor
      (list
        'begin
        (list
          'define
          (let-var-declr exp)
          (make-lambda (let-vars exp)
                       (let-body exp)))
        (list
          (let-var-declr exp)
          (let-values exp)))
      (list
        (make-lambda (let-vars exp)
                     (let-body exp)) 
        (let-values exp))))

