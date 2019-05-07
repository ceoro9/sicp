(define (make-interval x y) (cons x y))

(define (lower-bound x) (min (car x) (cdr x)))
(define (upper-bound x) (max (cdr x) (cdr x)))

(define (interval-width x)
    (abs (/ (- (upper-bound x) (lower-bound x)) 2.0)))

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y)) 
                   (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
   (make-interval (- (lower-bound x) (upper-bound y)) 
                  (- (upper-bound x) (lower-bound y))))

(define i-1 (make-interval 4 7))
(define i-2 (make-interval 7 13))

(display (interval-width i-1))
(newline)
(display (interval-width i-2))

(newline)
(newline)

(display (interval-width (add-interval i-1 i-2)))
(newline)
(display (interval-width (sub-interval i-1 i-2)))


