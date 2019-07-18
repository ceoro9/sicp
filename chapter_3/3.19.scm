(define (same-pairs? p1 p2)
    (let ((tmp1 (car p1))
          (answer '()))
      (set-car! p1 'wow)
      (if (eq? (car p2) 'wow)
          (set! answer #t)
          (set! answer #f))
      (set-car! p1 tmp1)
       answer))

; Floyd's Tortoise and Hare Cycle Detection
; https://en.wikipedia.org/wiki/Cycle_detection

(define (cycle? p)

    (define (double-cdr s)
        (let ((tmp (cdr s)))
            (if (null? tmp)
                '()
                (cdr tmp))))
    
    (define (iterate i seq-1 seq-2)
        (cond ((or (null? seq-1)
                   (null? seq-2))
               '())     
              ((same-pairs? seq-1 seq-2) i)
              (else (iterate (+ i 1)
                             (cdr seq-1)
                             (double-cdr seq-2)))))

    (not (null? (iterate 1 p (cdr p)))))

(define el (list 1 2 3))
(set-cdr! (cdr el) el)

(display (cycle? el))
(newline)

(display (cycle? (list 1 2 3)))
(newline)

