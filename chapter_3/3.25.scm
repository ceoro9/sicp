(define (make-table same-key?)
    (let ((local-table (list '*table*)))

        (define (assoc key records)
            (cond ((null? records) #f)
                  ((null? (car records)) #f)
                  ((null? (caar records)) #f)
                  ((same-key? key (caar records)) (car records))
                  (else (assoc key (cdr records)))))

        (define (look-up keys)
            (define (iterate i-keys records)
                (if (null? i-keys)
                    records
                    (let ((result (assoc (car i-keys) records)))
                      (if result
                          (iterate (cdr i-keys) (cdr result))
                          #f))))
            (iterate keys (cdr local-table)))

        (define (insert! keys value)
            (define (iterate i-keys record)
                (if (null? i-keys)
                    (set-cdr! record value)
                    (let ((result (assoc (car i-keys) (cdr record))))
                      (if result
                          (iterate (cdr i-keys) (cdr result))
                          (if (null? (cdr record))
                              ; create new record with key
                              (begin
                                (set-cdr! record (list (cons (car i-keys) '())))
                                (iterate (cdr i-keys) (car (cdr record))))
                              ; add our record to existing list 
                              (begin
                                (set-cdr! record (cons (cons (car i-keys) '()) (cdr record)))
                                (iterate (cdr i-keys) (car (cdr record)))))))))
            (iterate keys local-table)) 

        (define (print)
            (display local-table))
        
        (define (dispatch p)
            (cond ((eq? p 'look-up) look-up)
                  ((eq? p 'insert!) insert!)
                  ((eq? p 'print) print)
                  (else (error "No such operation" p))))
                  
        dispatch))

(define (look-up table keys) ((table 'look-up) keys))
(define (insert! table keys value) ((table 'insert!) keys value))
(define (print table) ((table 'print)))

(define table (make-table =))
(insert! table (list 1 2) "NICE")
(insert! table (list 2 3 4) "FOO")

(display (look-up table (list 1 2)))
(newline)
(display (look-up table (list 2 3 4)))
