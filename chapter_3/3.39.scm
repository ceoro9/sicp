(define x 10)
(define s (make-serializer))
(parallel-execute
  (lambda () (set! x ((s (lambda () (* x x))))))
  (s (lambda () (set! x (+ x 1)))))

; 1) ((s (lambda () (* x x)))) computes 100, function is completed and then context is switched
; 2) (s (lambda () (set! x (+ x 1))))) is executed freely,
;    'cause lock was released on previous step. It sets x to 11.
; 3) execuation on step 1 is resumed and it sets x to 100.
;
; So the result value of x is 100, which is abviously incorrect.
; The last option is remained.

