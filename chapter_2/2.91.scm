(define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                    ; subtract-terms and mul-terms
                    ; should be implemented in term package,
                    ; take a look on 2.90 exercise.
                    ; Plese do it on ur own.
                    (div-term (subtract-terms 
                                  L1
                                  (mul-terms (make-term new-o new-c) L2)))
                              L2))
                    (list
                        (adjon-term (make-term new-o new-c)
                                    (car rest-of-result))
                        (cadr rest-of-result))))))))

(define (div-poly P1 P2)
    (if (equal? (variable P1) (variable P2))
        (let ((result (div-terms (term-list P1)
                                 (term-list P2))))
            (list
                (make-poly (variable P1) (car result))
                (make-poly (variable P2) (cdr result))))
        (error "cannot divide polynomials with different variable")))

