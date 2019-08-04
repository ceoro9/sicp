(define (average a b) (/ (+ a b) 2))

(define (average-smooth s)
    (stream-map
      average
      s
      (stream-cdr s)))

(define (make-zero-crossings input-stream smooth)
    (let ((smoothed-stream (smooth input-stream)))
      (stream-map
       sign-change-detector
       smoothed-stream
       (cons 0 smoothed-stream))))
    
(make-zero-crossings sense-data average-smooth)

