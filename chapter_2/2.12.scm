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

(define (make-center-width c w)
    (make-interval (- c w) (+ c w)))

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2.0))

(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2.0))

(define (make-center-percent center inflecity)
    (define lower (- center (* center inflecity)))
    (define upper (- (* 2 center) lower))
    (make-interval lower upper))

(define (percent x)
    (/ (width x) (center x)))

(display (make-center-percent 5 0.1))
(newline)
(display (percent (make-interval 4.5 5.5)))
