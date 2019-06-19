; A) Splits elements on 3 parts: 
;    left_part <-> middle element <-> right_part
;    
;    Where all elements from left_part are less than 
;    middle element and all elements from right_part 
;    are greater, so in that case middle element become
;    a root element in current sub-tree and than we 
;    recursively making sub-sub-trees from left and
;    right parts and joining them with middle elements.
;    This functionality was implemented with not really
;    intuative function partial-tree, which accepts elements
;    and integer, of how many elements should be converted
;    to balanced treee and returns a pair, where first element
;    is tree and the second one is the remaining elements, but
;    such implementation allows us flexibly operate on given
;    elements and convert slices to balanced tree to later join them
;    to make a balanced sub-tree.
;
; B) O(N)
;    Since we are having 2 recursive calls from each element
;    and we have constant time to handle each one
;    (slice elements and join sub-trees). 
;

(define (make-tree entry left-branch right-branch)
    (list entry left-branch right-branch))

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


(display (list->tree (list 1 2 3 4 5 6)))

