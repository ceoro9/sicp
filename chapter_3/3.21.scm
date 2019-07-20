(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item)
    (set-car! queue item))

(define (set-rear-ptr! queue item)
    (set-cdr! queue item))

(define (empty-queue? queue)
    (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
    (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))

(define (insert-queue! queue item)
    (let ((new-pair (cons item '())))
      (cond ((empty-queue? queue)
             (set-front-ptr! queue new-pair)
             (set-rear-ptr! queue new-pair)
             queue)
            (else
              (set-cdr! (rear-ptr queue) new-pair)
              (set-rear-ptr! queue new-pair)
              queue))))

(define (delete-queue! queue)
    (cond ((empty-queue? queue)
           (error "DELETE! called with an empty queue" queue))
          (else (set-front-ptr! queue (cdr (front-ptr queue)))
                queue)))

(define (print-queue queue)
    (define (iterate l)
        (if (null? l)
            '()
            (cons (car l) (iterate (cdr l)))))
    (display (iterate (front-ptr queue))))


(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)

(print-queue q1)
(newline)

(delete-queue! q1)
(delete-queue! q1)

(print-queue q1)
(newline)
; b is printed, because rear-ptr has not been changed after
; that last element was removed from queue, since when 
; there is only one element is left in queue, front
; and rear pointers are pointing to the same element, but
; in our delete-queue! implementation we only reset value of
; front-ptr, so in case of 1 element, rear-ptr is still pointing the last element.
(display q1)
