(define (square x) (* x x))

(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (random range))))

(define (monte-carlo trials experiment)
    (define (iterate trials-remaining trials-passed)
        (cond ((= trials-remaining 0) (/ trials-passed trials))
              ((experiment)
               (iterate (- trials-remaining 1)
                        (+ trials-passed 1)))
              (else
               (iterate (- trials-remaining 1)
                        trials-passed))))
    (iterate trials 0))

(define (estimate-integral P x1 y1 x2 y2 trials)
    (monte-carlo
      trials
      (lambda ()
        (let ((c-x (random-in-range x1 x2))
              (c-y (random-in-range y1 y2)))
          (P c-x c-y)))))

(define (point-in-circle? c-x c-y r)
    (lambda (x y)
        (<= (+ (square (- x c-x))
               (square (- y c-y)))
            (square r))))

(define (estimate-pi trials)
    (let ((x1 2)
          (y1 4)
          (x2 8)
          (y2 10)
          (c-x 5)
          (c-y 7)
          (R 3))
      (let ((rectangle-area
            (* (abs (- x1 x2))
               (abs (- y1 y2)))))
        (let ((circle-area
              (* rectangle-area
                 (estimate-integral
                   (point-in-circle? c-x c-y R)
                   x1 y1 x2 y2 trials))))
          (exact->inexact (/ circle-area (square R)))))))

(display (estimate-pi 100000))

