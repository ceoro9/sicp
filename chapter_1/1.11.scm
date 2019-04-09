(define (rec-func n)
  (cond ((< n 3) n) 
        (else  (+ (func (- n 1)) 
                  (+  (func (- n 2)) 
                      (func (- n 3)))))))

(define (iter-func n)
  (cond ((< n 3) n)
        (else (iter-iter-func 1 0 n))))

(define (iter-iter-func i sum n)
  (cond ((> i n) sum) 
        (else (   ))) ;; TODO
)

(display (iter-func 5))