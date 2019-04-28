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

; --------------------------
; -------- Segment ---------
; --------------------------

(define (make-segment start-point end-point)
    (cons start-point end-point))

(define (start-segment s)
    (car s))

(define (end-segment s)
    (cdr s))

(define (midpoint-segment s)
    (define (abs x)
        (if (> x 0)
            x
            (* -1 x)))
    
    (define (mid v1 v2)
        (if (> v1 v2)
            (- v1 (/ (abs (- v1 v2)) 2))
            (- v2 (/ (abs (- v1 v2)) 2))))

    (make-point (mid (x-point (end-segment s)) (x-point (start-segment s))) 
                (mid (y-point (end-segment s)) (y-point (start-segment s)))))


(print-point (midpoint-segment (make-segment (make-point -1 -1) (make-point 5 0))))




