(define (make-mobile left right)
    (list left right))

(define (make-branch length structure)
    (list length structure))

(define (left-branch mobile)
    (car mobile))

(define (right-branch mobile)
    (car (cdr mobile)))

(define (branch-length branch)
    (car branch))

(define (branch-structure branch)
    (car (cdr branch)))

(define (total-branch-weight branch)
    (define (iter b)
        ; TODO: create is-branch?
        (if (list? (branch-structure b)) ; if mobile
            (total-mobile-weight (branch-structure b))
            (branch-structure b)))
    (iter branch))

(define (total-mobile-weight mobile)
    (+
        (total-branch-weight (left-branch mobile))
        (total-branch-weight (right-branch mobile))))

(define (is-balanced mobile)
    ; TODO: create is-modile?
    (if (list? mobile)
        (AND
            (=
                (* (branch-length (left-branch mobile))
                   (total-branch-weight (left-branch mobile)))
                (* (branch-length (right-branch mobile))
                    (total-branch-weight (right-branch mobile))))
           (is-balanced (branch-structure (left-branch mobile)))
           (is-balanced (branch-structure (right-branch mobile))))
        #t))


(define a (make-mobile (make-branch 4 5)
                       (make-branch 2 10)))

(define b (make-mobile (make-branch 1 (make-mobile (make-branch 5 6) (make-branch 0 4)))
                       (make-branch 2 10)))


(display (total-mobile-weight a))
(newline)
(display (total-mobile-weight b))

(newline)
(display "--------------------")
(newline)

(display (is-balanced a))
(newline)
(display (is-balanced b))
