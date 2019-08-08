(define (make-while-loop predicate body) (list 'while predicate body))
(define (while-loop? expr) (tagged-list expr 'while))
(define (while-predicate expr) (cadr expr))
(define (while-body expr) (caddr expr))

(define (while->combination expr)
    (sequance->exp
      (list
        (list
          'define
          'while-loop
          (make-if (while-predicate expr)
                   (sequance->exp
                     (list
                       (while-body expr)        ; execute body
                       (list 'while-loop '()))) ; next iteration
                   ; exit loop
                   'true))
        ; initial call
        (list 'while-loop '()))))

