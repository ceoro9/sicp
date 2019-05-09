(define r (list 1 (list 2 (list 3 4))))

; extract 1
(display (car r))

(newline)

; extract 2
(display (car (car (cdr r))))
