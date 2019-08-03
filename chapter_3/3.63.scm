(define (sqrt-improve guess x)
    (average guess (/ x guess)))

(define (sqrt-stream-1 x)
    (define guesses
      (cons-stream
       1.0
       (stream-map (lambda (guess) (sqrt-improve guess x))
                   guesses)))
    guesses)

(define (sqrt-stream-2 x)
    (cons-stream 1.0 (stream-map
                       (lambda (guess)
                         (sqrt-improve guess x))
                       (sqrt-stream-2 x))))

; > Alyssa P. Hacker replies that this version of the procedure
; > is considerably less efficient because it performs redundant
; > computation. Explain Alyssa’s answer
;
; Second version of sqrt-stream has recursive call, even if its result is memoized
; it does really matter, because every new recursive call creates a new stream and
; we are loosing all previously computed values, because it's just basically a new stream.
; And to compute them one more time we need to make N - 1 recursive calls
; on each iteration, so overall growth is N^2. Unlike the first version of sqrt-stream
; which operates on the single stream, thefore all computed values are memoized and the
; overall growth is just N.
;
;
; > Would the two versions still differ in efficiency if our implementation of delay
; > used only (lambda () ⟨ exp ⟩ ) without using the optimization
; > provided by memo-proc (Section 3.5.1)?
;
; No, because with no memoization the first version of sqrt-stream would have to compute
; all previous values one more time, so this is N^2 growth as well as second version has.
;

