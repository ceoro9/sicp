(define (addjoin-set x set)
    (cond ((null? set) (list x))
          ((> x (car set) )
              (cons (car set) (addjoin-set x (cdr set))))
          (else (cons x set))))


(display (addjoin-set 2 (list 1 3 10)))

