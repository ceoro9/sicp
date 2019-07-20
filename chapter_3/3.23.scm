(define (make-dequeue)
    (let ((front-ptr '())
          (rear-ptr '()))
      
      (define (make-pair data prev-item next-item)
          (list data prev-item next-item))

      (define (pair-data p) (car p))
      (define (prev-item p) (cadr p))
      (define (next-item p) (caddr p))
      (define (set-prev-item! p item) (set-car! (cdr p) item))
      (define (set-next-item! p item) (set-car! (cddr p) item))

      (define (empty-dequeue?) (null? front-ptr))

      (define (front-dequeue)
          (if (empty-dequeue?)
              (error "FRONT called with an empty queue")
              (pair-data front-ptr)))
      
      (define (rear-dequeue)
          (if (empty-dequeue?)
              (error "FRONT called with an empty queue")
              (pair-data rear-ptr)))
      
      (define (front-insert-dequeue! data)
          (if (empty-dequeue?)
              (let ((new-pair (make-pair data '() '())))
                (set! front-ptr new-pair)
                (set! rear-ptr new-pair))
              (let ((new-pair (make-pair data '() front-ptr)))
                (set-prev-item! front-ptr new-pair)
                (set! front-ptr new-pair))))

      (define (rear-insert-dequeue! data)
          (if (empty-dequeue?)
              (let ((new-pair (make-pair data '() '())))
                (set! front-ptr new-pair)
                (set! rear-ptr new-pair))
              (let ((new-pair (make-pair data rear-ptr '())))
                (set-next-item! rear-ptr new-pair)
                (set! rear-ptr new-pair))))
      
      (define (front-delete-dequeue!)
          (if (empty-dequeue?)
              (error "DELETE! called with an empty dequeue")
              (set! front-ptr (next-item front-ptr))))
      
      (define (rear-delete-dequeue!)
          (if (empty-dequeue?)
              (error "DELETE! called with an empty dequeue")
              (set! rear-ptr (prev-item rear-ptr))))

      (define (dispatch m)
          (cond ((eq? m 'empty-dequeue?) empty-dequeue?)
                ((eq? m 'front-dequeue) front-dequeue)
                ((eq? m 'rear-dequeue) rear-dequeue)
                ((eq? m 'front-insert-dequeue!) front-insert-dequeue!)
                ((eq? m 'rear-insert-dequeue!) rear-insert-dequeue!)
                ((eq? m 'front-delete-dequeue!) front-delete-dequeue!)
                ((eq? m 'rear-delete-dequeue!) rear-delete-dequeue!)))
      
      dispatch))

(define (empty-dequeue? q) ((q 'empty-dequeue?)))
(define (front-dequeue q) ((q 'front-dequeue)))
(define (rear-dequeue q) ((q 'rear-dequeue)))
(define (front-insert-dequeue! q data) ((q 'front-insert-dequeue!) data))
(define (rear-insert-dequeue! q data) ((q 'rear-insert-dequeue!) data))
(define (front-delete-dequeue! q) ((q 'front-delete-dequeue!)))
(define (rear-delete-dequeue! q) ((q 'rear-delete-dequeue!)))


(define lol (make-dequeue))
(front-insert-dequeue! lol 1)

(display (front-dequeue lol))
(newline)
(display (rear-dequeue lol))


(front-insert-dequeue! lol 2)

(newline)
(newline)

(display (front-dequeue lol))
(newline)
(display (rear-dequeue lol))


(rear-insert-dequeue! lol 3)

(newline)
(newline)

(display (front-dequeue lol))
(newline)
(display (rear-dequeue lol))

(front-delete-dequeue! lol)
(rear-delete-dequeue! lol)
(newline)
(display (front-dequeue lol))
(display "   ")
(display (rear-dequeue lol))


