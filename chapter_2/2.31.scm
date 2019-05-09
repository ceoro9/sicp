(define (square x) (* x x))

(define (tree-map proc tree)
    (map (lambda (sub-tree) 
            (if (NOT (pair? sub-tree))
                (proc sub-tree)
                (tree-map proc sub-tree))) 
    tree))

(define (square-tree tree)
    (tree-map square tree))


(display (square-tree (list 1 2 3 (list 5 6 (list 7) 8))))
