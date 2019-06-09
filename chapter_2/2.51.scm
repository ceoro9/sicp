(define (below-1 painter1 painter2)
    (define split-point (make-vect 0.0 0.5))
    (define paint-bottom (transform-painter painter1
                                            (make-vect 0.0 0.0)
                                            (make-vect 1.0 0.0)
                                            split-point))
    (define paint-top (transform-painter painter2
                                         split-point
                                         (make-vect 0.5 1.0)
                                         (make-vect 0.0 1.0)))
    (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))

(define (below-2 painter1 painter2)
    (rotate270 (beside (rotate90 painter1) (rotate90 painter2))))
