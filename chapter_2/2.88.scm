(define (install-polynomial-package)

    ; ,,,
 
    (define (subtract-poly p1 p2)
        (add-poly p1
                  ; multiply p2 on -1 polynomу with zero indeterminate
                  (mul-poly (make-poly (variable p2)                                                                                                                             
                                       (list (make-term 0 -1)))
                            p2)))

    (put 'subtract '(polynomial polynomial) subtract-poly))

