; ------------------------
; -------- Point ---------
; ------------------------

(define (make-point x y)
    (cons x y))

(define (x-point p)
    (car p))

(define (y-point p)
    (cdr p))

(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ", ")
    (display (y-point p))
    (display ")")
    (newline))

; -----------------------------
; --------- Rectangle ---------
; -----------------------------

(define (make-rectangle start-point end-point)
    (cons start-point end-point))

(define (start-segment s)
    (car s))

(define (end-segment s)
    (cdr s))

(define (abs x)
    (if (> x 0)
        x
        (* -1 x)))

(define (perimeter r)
    (let ((p1 (start-segment r)) 
          (p2 (end-segment r)))
        (* 2 
           (+ (abs (- (x-point p1) 
                      (x-point p2)))
              (abs (- (y-point p1) 
                      (y-point p2)))))))

(define (square r)
    (let ((p1 (start-segment r))
          (p2 (end-segment r)))
        (*
            (abs (- (x-point p1) (x-point p2)))
            (abs (- (y-point p1) (y-point p2))))))

(display (perimeter (make-rectangle (make-point -1 -1) (make-point 10 10))))
(newline)
(display (square (make-rectangle (make-point -1 -1) (make-point 10 10))))
