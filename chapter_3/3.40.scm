(define x 10)
(parallel-execute
  (lambda () (set! x (* x x))) ; P1
  (lambda () (set! x (* x x x)))) ; P2

; 1000: P1 and P2 read x, P1 sets x to 100, P2 sets x to 1000.
; 100: P1 and P2 read x, P2 sets x to 1000, P1 sets x to 100.
; 1000000: P1 sets x to 100, P2 reads new x and sets it to 1000000.
; 1000000: P2 sets x to 1000, P2 reads new x and sets it to 1000000.
; 10000: P1 reads x(10), P2 sets x to 1000, P1 reads x(1000) and sets it to 10000.
; 10000: P2 reads x(10) twice, P1 sets x to 100, P2 reads x(100) and sets it to 10000.
; 10000: P2 reads x(10), P1 sets x to 100, P2 reads x(100) twice and sets it to 100000.

(define x 10)
(define s (make-serializer))
(parallel-execute 
  (s (lambda () (set! x (* x x)))) ; P1
  (s (lambda () (set! x (* x x x))))) ; P2

; 1000000: P1 sets x to 100, P2 reads new x and sets it to 1000000.
; 1000000: P2 sets x to 1000, P2 reads new x and sets it to 1000000.

