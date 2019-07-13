(define (make-monitored f)
    (define counter 0)
    (lambda args
        (cond ((= (length args) 0)
            (begin
                (set! counter (+ counter 1))
                (apply f args)))
              ((eq? (car args) 'how-many-calls?) counter)
              ((eq? (car args) 'reset-count) (set! counter 0))
              (else
                (begin
                  (set! counter (+ counter 1))
                  (apply f args))))))

(define proc (make-monitored sqrt))

(display (proc 9))
(newline)
(display (proc 36))
(newline)
(display (proc 'how-many-calls?))
(newline)
(proc 'reset-count)
(display (proc 'how-many-calls?))

