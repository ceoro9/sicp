(define (for-each proc items)
    (if (null? items)
        #t
        (begin
            (proc (car items))
            (for-each proc
                     (cdr items)))))

(for-each (lambda (x)
            (begin 
                (display x)
                (display "\n"))) 
          (list 1 2 3))
