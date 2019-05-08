(define (square-list items) 
    (define (iter things answer) 
        (if (null? things) 
             answer 
             (iter (cdr things) 
                   (cons answer (square (car things)))))) 
    (iter items '())) 
                                              
(display (square-list (list 1 2 3 4))) ; slighly wrong representation, but it's iterative process
