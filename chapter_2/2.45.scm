(define (split main-proc split-proc)
    (lambda (painter n) 
        (if (= n 1)
            painter
            (let ((smaller ((split main-proc split-proc) painter (- n 1))))
                (main-proc painter (split-proc smaller smaller))))))


(define right-split (split beside below))

(define up-split (split below beside))
