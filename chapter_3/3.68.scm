(define (pairs s t)
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x)) t)
      (pairs (stream-cdr s) (stream-cdr t))))

; does not work, because there is no check on empty stream
(define result
    (pairs (cons-stream 1 (cons-stream 2 '()))
           (cons-stream 1 (cons-stream 2 '()))))

; infinite recursion, because pairs recursive call
; is not delayed, since it's not under cons-stream
(define result (pairs integers integers))

