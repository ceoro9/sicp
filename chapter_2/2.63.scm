(define (make-tree entry left-branch right-branch)
    (list entry left-branch right-branch))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (tree->list-1 tree)
    (if (null? tree)
        '()
        (append (tree->list-1 (left-branch tree))
                (cons (entry tree)
                      (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
        (if (null? tree)
            result-list
            (copy-to-list (left-branch tree)
                          (cons (entry tree)
                                (copy-to-list (right-branch tree)
                                              result-list)))))
    (copy-to-list tree '()))


; A) same result because they are both same variation of DFS,
;    but tree->list-1 is recursive and tree->list-2 is iterative.

; B) O(N log N) and O(N)

(display (tree->list-1 (make-tree 10
                                  (make-tree 5
                                             (make-tree 3 '() '())
                                             (make-tree 7 '() '()))
                                  (make-tree 15 '() '()))))

(newline)

(display (tree->list-2 (make-tree 10
                                  (make-tree 5
                                             (make-tree 3 '() '())
                                             (make-tree 7 '() '()))
                                  (make-tree 15 '() '()))))
