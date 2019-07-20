(define (make-queue)
    (let ((front-ptr '())
          (rear-ptr '()))
      
      (define (empty-queue?) (null? front-ptr))

      (define (front-queue)
          (if (empty-queue?)
              (error "FRONT called with an empty queue")
              (car front-ptr)))
      
      (define (insert-queue! item)
          (let ((new-pair (cons item '())))
            (if (empty-queue?)
                (begin
                  (set! front-ptr new-pair)
                  (set! rear-ptr new-pair))
                (begin
                  (set-cdr! front-ptr new-pair)
                  (set! rear-ptr new-pair)))))
      
      (define (delete-queue!)
          (if (empty-queue?)
              (error "DELETE! called with an empty queue")
                (set! front-ptr (cdr front-ptr))))

      (define (dispatch m)
          (cond ((eq? m 'empty-queue?) empty-queue?)
                ((eq? m 'front-queue) front-queue)
                ((eq? m 'insert-queue!) insert-queue!)
                ((eq? m 'delete-queue!) delete-queue!)))
      
      dispatch))

(define (empty-queue? q) ((q 'empty-queue?)))
(define (front-queue q) ((q 'front-queue)))
(define (insert-queue! q item) ((q 'insert-queue!) item))
(define (delete-queue! q) ((q 'delete-queue!)))


(define lol (make-queue))
(insert-queue! lol 1)
(insert-queue! lol 2)
(display (front-queue lol))
(newline)
(delete-queue! lol)
(display (front-queue lol))
(delete-queue! lol)
(newline)
(display (front-queue lol)) ; Error

