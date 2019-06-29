(define (accumulate op base func ls)
    (if (null? ls)
        base
        (op (func (car ls))
            (accumulate op base func (cdr ls)))))

(define (apply-generic op . args)
    
    ; returns true if all args are castalbe to current-type
    (define (check-if-all-args-are-castable-to-type current-type current-args)
        (cond ((null? current-args) #t)
              ((or
                  (equal? current-type
                         (type-tag (car current-args)))
                  (get-coersion (type-tag (car current-args))
                                current-type))
                      (check-if-all-args-are-castable-to-type
                           current-type
                           (cdr current-args)))
              (else #f)))
    
    ; returns type all args are castable to
    (define (find-type-to-cast current-args)
        (if (null? current-args)
            #f
            (let ((result
                    (check-if-all-args-are-castable-to-type
                        (type-tag (car current-args))
                        args)))
                 (if result
                     (type-tag (car current-args))
                     (find-type-to-cast (cdr current-args))))))
    
    (let ((type-tags (map type-tag args)))
        (let ((proc (op type-tags)))
            (if proc
                (apply proc (map contents args))
                (let ((type-to-cast (find-type-to-cast args)))
                    (if (type-to-cast)
                        (accumulate
                            (get-coersion type-to-cast
                                          type-to-cast)
                            (car args)
                            (lambda (el) ((get-coersion (type-tag el) type-to-cast) el))
                            (cdr args))
                        (error "No method for this types" (list op type-tags))))))))

; Example when this strategy does not work:
; Suppose we have 4 types of args: A, B, C, D
; - A is castable only to D (A->D)
; - B->C, B->D
; - C->B
; - D->A
;
; So as you see there is no such type all others arguments can be casted to.
; But we can cast A to D and apply operation on them. Then cast B to D and apply 
; operation on it and result of previos operation. And finally cast C to B and then
; to D and apply a final operation ... So technically all args were casted to D type,
; but for C type there were no way to do this directly as far as our algorithm cannot
; detect such kind of relation ships, it's not going to work in given test case.
;
; (((A->D D) B->D) C->B->D) 
