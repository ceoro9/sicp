(define (make-interval x y) (cons x y))

(define (lower-bound x) (min (car x) (cdr x)))
(define (upper-bound x) (max (cdr x) (cdr x)))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p2 p3 p4) 
                       (max p1 p2 p3 p4))))

(define (is-negative-interval? x)
    (AND (< (lower-bound x) 0) (< (upper-bound x) 0)))

(define (is-positive-interval? x)
    (AND (> (lower-bound x) 0) (> (upper-bound x) 0)))

(define (is-crossed-interval? x)
    (AND (<= (lower-bound x) 0) (>= (upper-bound x) 0)))

(define (new-mul-interval x y)
    (cond ((AND (is-positive-interval? x)
                (is-positive-interval? y))
          (make-interval (* (lower-bound x) (lower-bound y)) 
                         (* (upper-bound x) (upper-bound y))))
          
          ((AND (is-negative-interval? x)
                (is-negative-interval? y))
          (make-interval (* (upper-bound x) (upper-bound y))
                         (* (lower-bound x) (lower-bound y))))

          ((AND (is-negative-interval? x)
                (is-positive-interval? y))
          (make-interval (* (lower-bound x) (upper-bound y))
                         (* (upper-bound x) (lower-bound y))))

          ((AND (is-positive-interval? x)
                (is-negative-interval? y))
          (make-interval (* (lower-bound y) (upper-bound x))
                         (* (upper-bound y) (lower-bound x))))

          ((AND (is-positive-interval? x) 
                (is-crossed-interval? y))
          (make-interval (* (upper-bound x) (lower-bound y))
                         (* (upper-bound x) (upper-bound y))))

          ((AND (is-negative-interval? x) 
                (is-crossed-interval? y))
          (make-interval (* (lower-bound x) (upper-bound y))
                         (* (lower-bound x) (lower-bound y))))

          ((AND (is-positive-interval? y) 
                (is-crossed-interval? x))
          (make-interval (* (upper-bound y) (lower-bound x))
                         (* (upper-bound y) (upper-bound x))))

          ((AND (is-negative-interval? y) 
                (is-crossed-interval? x))
          (make-interval (* (lower-bound y) (upper-bound x))
                         (* (lower-bound y) (lower-bound x))))
          
          ((AND (is-crossed-interval? x) 
                (is-crossed-interval? y))
          (make-interval (min (* (lower-bound x) (upper-bound y))
                              (* (upper-bound x) (lower-bound y)))
                         (max (* (lower-bound x) (lower-bound y))
                              (* (upper-bound x) (upper-bound y)))))))

(display (mul-interval (make-interval -3 -5) (make-interval -5 -2)))
(newline)
(display (new-mul-interval (make-interval -3 -5) (make-interval -5 -2)))

(newline)

(display (mul-interval (make-interval -3 10) (make-interval -5 15)))
(newline)
(display (new-mul-interval (make-interval -3 10) (make-interval -5 15)))

(newline)

(display (mul-interval (make-interval 3 5) (make-interval 4 6)))
(newline)
(display (new-mul-interval (make-interval 3 5) (make-interval 4 6)))

(newline)

(display (mul-interval (make-interval -3 5) (make-interval 4 6)))
(newline)
(display (new-mul-interval (make-interval -3 5) (make-interval 4 6)))

(newline)

(display (mul-interval (make-interval 4 6) (make-interval -3 5)))
(newline)
(display (new-mul-interval (make-interval 4 6) (make-interval -3 5)))
