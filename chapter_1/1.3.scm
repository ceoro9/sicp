(define (max a b) (if (> a b ) a b))

(define (f a b c) (max a (max b c)))

(display (f 10 20 30))
(display ("\n"))
(display (f 300 20 30))