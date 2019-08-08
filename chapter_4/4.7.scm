(define (eval exp env)
    ; ...
    ((let*? exp) (eval (let*->nested-lets exp) env))
    ; ...
)


(define (make-let* inits body) (list 'let* inits body))
(define (let*? expr) (tagged-list? expr 'let*)) 
(define (let*-inits expr) (cadr expr))
(define (let*-vars expr) (map car (let*-inits expr))) 
(define (let*-values expr) (map cadr (let*-inits expr)))
(define (let*-body expr) (caddr expr))

(define (let*->nested-lets exp)
   (if (null? (let*-inits exp))
       (make-let '() (let*-body exp))
       (make-let
         (list (car (let*-inits exp)))
         (let*->nested-lets
           (make-let*
             (cdr (let*-inits exp))
             (let*-body exp))))))

