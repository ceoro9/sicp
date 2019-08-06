; !!! instead of simple car - better use standard type-tag procudure !!!
(define (eval exp env) ((get 'operation (type-tag exp)) exp env))

; example of registering operation
(put 'operation 'definition (lambda (exp env) (...)))

