; A)
(define (draw-outline frame)
    (define edges-sum (add-vect (edge1-frame frame) (edge2-frame)))
    (segments->painter
        (list
            (make-segment (origin-frame frame) (edge1-frame frame))
            (make-segment (origin-frame frame) (edge2-frame frame))
            (make-segment (edge1-frame frame) edges-sum)
            (make-segment (edge2-frame frame) edges-sum))))

; B)
(define (draw-x frame)
    (define edges-sum (add-vect (edge1-frame frame) (edge2-frame)))
    (segments->painter
        (list
            (make-segment (origin-frame frame) edges-sum))
            (make-segment (edge1-frame frame) (edge2-frame frame))))

; C)
(define (draw-diamond-shape frame)
    (define edges-sum-vect (add-vect (edge1-frame frame) (edge2-frame)))
    (define (half-vect v) (scale-vect 0.5 v))
    (define edge3-vect (sub-vect edges-sum-vect (edge1-frame frame)))
    (define edge4-vect (sub-vect edges-sum-vect (edge2-frame frame)))
    (segments->painter
        (list
            (make-segment (half-vect (edge1-frame frame))
                          (half-vect (edge2-frame frame)))
            
            (make-segment (half-vect (edge1-frame frame))
                          (half-vect edge3-vect))
            
            (make-segment (half-vect (edge2-fram frame))
                          (half-vect edge4-vect))

            (make-segment (half edge3-vect)
                          (half edge4-vect)))))

; D)
(define (draw-wave frame)
    (define frame-mapper (frame-coord-map frame))
    (lambda (image)
        (segments->painter
            (map (lambda (p)
                    (make-segment (frame-mapper p)
                                  (frame-mapper p)))
                 (list-image-points image)))))

