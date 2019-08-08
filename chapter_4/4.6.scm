(define (eval exp env)
    ; ...
    ((let? exp) (eval (let->combination exp) env))
    ; ...
)

(define (make-let inits body) (list 'let inits body))
(define (let? expr) (tagged-list? expr 'let)) 
(define (let-inits exps) (cadr expr))
(define (let-vars expr) (map car (let-inits expr))) 
(define (let-values expr) (map cadr (let-inits expr))) 
(define (let-body expr) (caddr expr))

(define (let->combination exp)
    (list
      (make-lambda (let-vars exp) (let-body exp)) 
      (let-values exp)))

