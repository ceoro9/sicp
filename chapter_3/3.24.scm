(define (make-table same-key?)
    (let ((local-table (list '*table*)))
      
      (define (assoc key records)
          (cond ((null? records) #f)
                ((same-key? key (caar records)) (car records))
                (else (assoc key (cdr records)))))

      (define (look-up key)
          (let ((record (assoc key (cdr local-table))))
            (if record
                (cdr record)
                #f)))
      
      (define (insert! key value)
          (let ((record (assoc key (cdr local-table))))
            (if record
                (set-cdr! record value)
                (set-cdr! local-table (cons (cons key value)
                                            (cdr local-table))))))

      (define (dispatch p)
          (cond ((eq? p 'look-up) look-up)
                ((eq? p 'insert!) insert!)
                (else (error "Unknow operation" p))))

      dispatch))

(define (look-up table key) ((table 'look-up) key))
(define (insert! table key value) ((table 'insert!) key value))

(define table (make-table equal?))

(insert! table 1 "hello")
(display (look-up table 1))

