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

(define (par1 r1 r2)
    (div-interval (mul-interval r1 r2)
                  (add-interval r1 r2)))

(define (par2 r1 r2)
    (let ((one (make-interval 1 1)))
        (div-interval one
                      (add-interval (div-interval one r1)
                                    (div-interval one r2)))))

(display (par1 (make-interval 10 10.1) (make-interval 100 101))) ; (9.00022277227723 . 9.272956795679569) 
(newline)
(display (par2 (make-interval 10 10.1) (make-interval 100 101))) ; (9.090909090909092 . 9.181818181818182) 
