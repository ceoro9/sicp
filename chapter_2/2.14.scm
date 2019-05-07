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

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y)) 
                   (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
    (make-center-percent (* (center x) (center y))
                         (+ (percent x) (percent y))))

(define (div-interval x y)
    (if (<= (* (lower-bound y) (upper-bound y)) 0)
        (error "Interval spans 0: " y)
        (mul-interval x
                       (make-interval (/ 1.0 (upper-bound y))
                                      (/ 1.0 (lower-bound y))))))


(define (test)
    (let ((i-1 (make-interval 20 21))
          (i-2 (make-interval 20 21)))

        (display (div-interval i-1 i-1)) ; should give [1;1], because in equation it's the same "number"
        (newline)
        (display (div-interval i-2 i-2)))) ; give the same result as expression above (different numbers(may be) in the same interval)

(test)


