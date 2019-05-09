(define (square x) (* x x))

(define (square-tree tree)
    (cond ((null? tree) '())
          ((NOT (pair? tree)) (square tree))
          (else (cons (square-tree (car tree))
                      (square-tree (cdr tree))))))

(define (map-square-tree tree)
    (map (lambda (sub-tree) 
            (if (NOT (pair? sub-tree))
                (square sub-tree)
                (map-square-tree sub-tree))) 
    tree))


(display (square-tree (list 1 (list 5 6) 3)))
(newline)
(display (map-square-tree (list 1 (list 5 6) 3)))
