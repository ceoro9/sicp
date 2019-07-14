(define f (let ((flag #f))
    (lambda (x)
      (if flag
          0
          (begin
            (set! flag #t)
            x)))))

(display (+ (f 0) (f 1)))
(newline)
(display (+ (f 1) (f 0)))

