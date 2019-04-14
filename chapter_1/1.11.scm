(define (rec-func n)
  (cond ((< n 3) n) 
    (else  (+ (rec-func (- n 1)) 
      (+  (* 2 (rec-func (- n 2))) 
      (* 3 (rec-func (- n 3))))))))


(define (iter-func n)
  (define (inner-iter-func p1 p2 p3 count)
    (if (= count 0) p1 (inner-iter-func p2 p3 (+ p3 (+ (* 2 p2) (* 3 p1))) (- count 1))))
  (inner-iter-func 0 1 2 n))

(display (iter-func 11))
(display "\n")
(display (rec-func 11))
(display "\n")

(define (iter-func n)
  (define (inner-iter-func p1 p2 p3 count)
