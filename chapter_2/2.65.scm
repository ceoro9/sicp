(define (make-tree entry left-branch right-branch)
    (list entry left-branch right-branch))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (tree->list tree)
    (define (copy-to-list tree result-list)
        (if (null? tree)
            result-list
            (copy-to-list (left-branch tree)
                          (cons (entry tree)
                                (copy-to-list (right-branch tree)
                                              result-list)))))
    (copy-to-list tree '()))

(define (list->tree elements)
    (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let ((left-size (quotient (- n 1) 2)))
          (let ((left-result (partial-tree elts left-size)))
            (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
              (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
                (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                    (cons (make-tree this-entry
                                     left-tree
                                     right-tree)
                          remaining-elts))))))))

(define (union-set tree1 tree2)
    (define (union-ordered-list-sets set1 set2)
        (define (handle s1 s2)
            (cond ((and (null? s1) (null? s2)) '())
                  ((null? s1) s2)
                  ((null? s2) s1)
                  ((< (car s1) (car s2))
                      (cons (car s1)
                          (handle (cdr s1) s2)))
                  ((> (car s1) (car s2))
                      (cons (car s2)
                          (handle s1 (cdr s2))))
                  ; current elements are equal
                  (else (cons (car s2)
                              (handle (cdr s1)
                                      (cdr s2))))))
        (handle set1 set2))


    (let ((elements1 (tree->list tree1))      ; O(N)
          (elements2 (tree->list tree2)))     ; O(N)
      (let ((unioned-list-set                 ; O(N)
                (union-ordered-list-sets elements1 elements2)))
          (list->tree unioned-list-set))))    ; O(N)


(define (intersection-set tree1 tree2)
    (define (intersection-ordered-list-sets set1 set2)
        (if (or (null? set1) (null? set2))
            '()
            (let ((x1 (car set1)) (x2 (car set2)))
                (cond ((= x1 x2) (cons x1 (intersection-ordered-list-sets (cdr set1) (cdr set2))))
                      ((< x1 x2) (intersection-ordered-list-sets (cdr set1) set2))
                      ((< x1 x1) (intersection-ordered-list-sets set1 (cdr set2)))))))

    (let ((elements1 (tree->list tree1))           ; O(N)
          (elements2 (tree->list tree2)))          ; O(N)
      (let ((intersectioned-list-set               ; O(N)
                (intersection-ordered-list-sets elements1 elements2)))
          (list->tree intersectioned-list-set))))  ; O(N)


(display (union-set (list->tree (list 1 2 3 4 5 6))
                    (list->tree (list 1 2 3 7))))

(newline)

(display (intersection-set (list->tree (list 1 2 3 4 5 6))
                           (list->tree (list 1 2 3 7))))
