; ---------------------------------------------------------------

(define (create-data-direct-map) '())

(define (make-data-direct-map-item operation type item) (list operation type item))
(define (select-operation dd-map-item) (car dd-map-item))
(define (select-type dd-map-item) (cadr dd-map-item))
(define (select-item dd-map-item) (caddr dd-map-item))

(define data-direct-map (create-data-direct-map))

(define (get-from-dd-map dd-map operation type)
    (cond ((null? dd-map) #f)
          ((and (eq? (select-operation (car dd-map))
                      operation)
                (eq? (select-type (car dd-map))
                      type)) (select-item (car dd-map)))
           (else (get-from-dd-map (cdr dd-map) operation type))))

(define (put-to-dd-map dd-map operation type item)
    (cons (make-data-direct-map-item operation
                                     type
                                     item)
          dd-map))

(define (put operation type item)
    (define new-dd-map (put-to-dd-map data-direct-map operation type item))
    (set! data-direct-map new-dd-map))

(define (get operation type)
    (get-from-dd-map data-direct-map
                     operation
                     type))

; ---------------------------------------------------------------

(define (get-record division-number emp-id)
    ((get 'get-record division-number) emp-id))

(define (get-salary division-number emp-id)
    ((get 'get-salary division-number) emp-id))

(define (find-employee division-number full-name)
    ((get 'find-employee division-number) full-name))

; ---------------------------------------------------------------

(define (install-division-1)
    
    (define (make-division-record emp-id full-name city salary)
        (cons emp-id (list full-name city salary)))
    
    (define (select-employee-id record) (car record))
    (define (select-full-name record) (car (cdr record)))
    (define (select-city record) (cadr (cdr record)))
    (define (select-salary record) (caddr (cdr record)))
    
    (define division-data (list
                            (make-division-record 1 "John" "New-York" 1000)
                            (make-division-record 2 "Jack" "Los-Angeles" 2000)
                            (make-division-record 3 "Katie" "Paris" 3000)))

    (define (iterate-over match? extract)
        (define (handle current-data)
            (if (null? current-data)
                #f
                (if (match? (car current-data))
                    (extract (car current-data))
                    (handle (cdr current-data)))))

        (handle division-data))

    (define (get-record emp-id)
        (iterate-over
            (lambda (employee) (= (select-employee-id employee) emp-id))
            (lambda (employee) employee)))

    (define (get-salary emp-id)
        (iterate-over
            (lambda (employee) (= (select-employee-id employee) emp-id))
            (lambda (employee) (select-salary employee))))

    (define (find-employee full-name)
        (iterate-over
            (lambda (employee) (equal? (select-full-name employee) full-name))
            (lambda (employee) employee)))
    
    (put 'get-record 'division-1 get-record)
    (put 'get-salary 'division-1 get-salary)
    (put 'find-employee 'division-1 find-employee))

; ---------------------------------------------------------------

(define (install-division-2)
    
    (define (make-division-record emp-id full-name city salary)
        (cons (list city full-name salary) emp-id))
    
    (define (select-employee-id record) (cdr record))
    (define (select-full-name record) (cadr (car record)))
    (define (select-city record) (car (car record)))
    (define (select-salary record) (caddr (car record)))

    (define division-data (list
                            (make-division-record 1 "John" "New-York" 1000)
                            (make-division-record 2 "Jack" "Los-Angeles" 2000)
                            (make-division-record 3 "Katie" "Paris" 3000)
                            (make-division-record 4 "MacKey" "Moscow" 8000)))
    
    (define (iterate-over match? extract)
        (define (handle current-data)
            (if (null? current-data)
                #f
                (if (match? (car current-data))
                    (extract (car current-data))
                    (handle (cdr current-data)))))

        (handle division-data))

    (define (get-record emp-id)
        (iterate-over
            (lambda (employee) (= (select-employee-id employee) emp-id))
            (lambda (employee) employee)))

    (define (get-salary emp-id)
        (iterate-over
            (lambda (employee) (= (select-employee-id employee) emp-id))
            (lambda (employee) (select-salary employee))))

    (define (find-employee full-name)
        (iterate-over
            (lambda (employee) (equal? (select-full-name employee) full-name))
            (lambda (employee) employee)))

    (put 'get-record 'division-2 get-record)
    (put 'get-salary 'division-2 get-salary)
    (put 'find-employee 'division-2 find-employee))

; ---------------------------------------------------------------

; Althought get-record, get-salary, find-employee for both divisions are the same,
; in fact they could be implemented absolutely different, but in our case for simplification
; I copy-paste the code.

(install-division-1)
(install-division-2)

(display (get-record 'division-1 1))
(newline)
(display (get-record 'division-2 1))

(newline)
(newline)

(display (get-salary 'division-1 2))
(newline)
(display (get-salary 'division-2 2))

(newline)
(newline)

(display (find-employee 'division-1 "John"))
(newline)
(display (find-employee 'division-2 "Kattie"))
