(define (make-table)
    
    (define (make-node key value left-branch right-branch)
        (list key value left-branch right-branch))
    (define (key node) (car node))
    (define (value node) (cadr node))
    (define (left-branch node) (caddr node))
    (define (right-branch node) (cadddr node))
    (define (set-value! node new-value) (set-car! (cdr node) new-value))
    (define (set-left-branch! node child) (set-car! (cddr node) child))
    (define (set-right-branch! node child) (set-car! (cdddr node) child))

    (let ((local-table '()))
        
        (define (look-up search-key)
            (define (iterate node)
               (cond ((null? node) #f)
                     ((= search-key (key node)) (value node))
                     ((> search-key (key node)) (iterate (right-branch node)))
                     (else (iterate (left-branch node)))))
            (iterate local-table))
        
        (define (insert! new-key value)
            (define (iterate prev-node current-node)
                (cond ((null? current-node)
                           (if (> new-key (key prev-node))
                               (set-right-branch! prev-node (make-node new-key value '() '()))
                               (set-left-branch! prev-node (make-node new-key value '() '()))))
                      ((= new-key (key current-node))
                           (set-value! current-node value))
                      ((> new-key (key current-node))
                           (iterate current-node (right-branch current-node)))
                      (else (iterate current-node (left-branch current-node)))))
            (if (null? local-table)
                (set! local-table (make-node new-key value '() '()))
                (iterate '() local-table)))

        (define (dispatch p)
            (cond ((eq? p 'look-up) look-up)
                  ((eq? p 'insert!) insert!)
                  (else (error "No such operation"))))

        dispatch))

(define (look-up table key) ((table 'look-up) key))
(define (insert! table key value) ((table 'insert!) key value))


(define table (make-table))

(insert! table 1 "NICE")
(insert! table 1 "NNNNICE")
(insert! table 2 "HELLO")
(insert! table -4 "GO")

(display (look-up table 1))
(newline)
(display (look-up table 2))
(newline)
(display (look-up table -4))

