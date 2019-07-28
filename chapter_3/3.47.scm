; A)
(define (make-semaphore n)
                   (if (> count 0)
    (let ((mutex (make-mutex))
          (count 0))
      (define (the-semaphore m)
          (cond ((eq? m 'acquire)
                 (begin 
                   (mutex 'acquire)
                   (if (< count n)
                       (begin
                         (set! count (+ count 1))
                         (mutex 'release))
                       (begin
                         (mutex 'release)
                         (the-semaphore m))))) 
                ((eq? m 'release)
                 (begin
                   (mutex 'acquire)
                   (if (not (= count 0))
                       (begin
                         (set! count (- count 1)))
                         (mutex 'release))
                       (begin
                         (mutex 'release)
                         (error "Cannot release not acquired semaphore"))
                   (mutex 'release)))
                (else (error "Uknown operation on semaphore" m))))))

; B)
(define (make-semaphore n)
    (let ((count 0))
      (define (the-semaphore m)
          (cond ((eq? m 'acquire)
                 (if (test-and-set! count 1 n)
                     (the-semaphore m)))
                ((eq? m 'release)
                 (if (test-and-set! count -1 n)
                     (error "Cannot release not acquired semaphore"))
                     #t)))
      the-semaphore))

; Atomic operation
(define (test-and-set! count value n)
    (if (or (> (+ count value) n)
            (< (+ count value) 0))
        #t
        (begin
          (set! count (+ count value))
          #f)))

