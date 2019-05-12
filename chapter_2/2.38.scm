(define (fold-left p start-value sequance)
    (define (iter c result)
        (if (null? c)
            result
            (iter (cdr c)
                  (p result (car c)))))
    (iter sequance start-value))

 (define (fold-right op initial sequence) 
    (if (null? sequence) 
        initial 
        (op (car sequence) 
            (fold-right op initial (cdr sequence))))) 

; division is not communicative
(display (fold-left / 1 (list 1 2 3)))
(newline)
(display (fold-right / 1 (list 1 2 3)))

(newline)
(newline)
(newline)

; but addition is
(display (fold-left + 1 (list 1 2 3)))
(newline)
(display (fold-right + 1 (list 1 2 3)))


; Property of equation: op should be commincative.
