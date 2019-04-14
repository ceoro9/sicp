(define (square x) (* x x))

(define (even? n) (= (remainder n 2) 0))

(define (iter-fast-expt b n a)
    (cond ((= n 1) a) 
          (else  
            (cond ((even? n)  
                  (iter-fast-expt b (/ n 2) (square a))) 
                  (else (iter-fast-expt b (/ (- n 1) 2) (* b (square a)) )) ) )))

(define (fast-expt-iter b n)
    (iter-fast-expt b n b))


(display (fast-expt-iter 2 10))
