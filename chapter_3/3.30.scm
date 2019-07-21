; Total delay: full-adder-delay * n
(define (ripple-carry a-wires b-wires s-wires c-wire)
    (define (iterate a b s c-in c-out)
        (if (null? a)
            s-wires
            (begin
              (full-adder (car a) (car b) c-in (car s) c-out)
              (iterate (cdr a) (cdr b) (cdr s) c-in (make-wire)))))
    (iterate a-wires b-wires s-wires c-wire (make-wire)))

