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


(define (fib n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (memoize f)
    (let ((table (make-table =)))
      (lambda (x)
        (let ((previously-computed-result (look-up table x)))
          (or previously-computed-result
            (let ((result (f x)))
              (insert! table x result)
              result))))))

; memoization works for all calls (linear computation)
(define memo-fib-1
  (memoize
  (lambda (n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (memo-fib-1 (- n 1))
                   (memo-fib-1 (- n 2))))))))

(display (memo-fib-1 10))
(newline)
(display (memo-fib-1 10)) ; memoization works
(newline)
(display (memo-fib-1 9))  ; and there works too, because memoized functions where called recursively

(newline)
(newline)
(newline)

; memoization works only for param value in first call (exponentiation computation)
(define memo-fib-2 (memoize fib))
(display (memo-fib-2 10))
(newline)
(display (memo-fib-2 10)) ; memoization works
(newline)
(display (memo-fib-2 9))  ; and now does not, although fib(9) where computed on previous step.
                          ; It works like this because non-memoized fib where called recursively,
                          ; so fib(9) where not memorized when memo-fib-2(10) where computed.
