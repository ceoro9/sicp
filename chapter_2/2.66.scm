(define (key entry) entry)

(define (make-tree entry left-branch right-branch)
    (list entry left-branch right-branch))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

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

(define (lookup given-key tree)
    (if (null? tree)
        #f
        (let ((entry-key (key (entry tree))))
            (cond ((= given-key entry-key) (entry tree))
                  ((< given-key entry-key) (lookup given-key (left-branch tree)))
                  (else (lookup given-key (right-branch tree)))))))

(display (lookup 10 (list->tree (list 1 2 3 4 5))))
(newline)
(display (lookup 5 (list->tree (list 1 2 3 4 5))))
