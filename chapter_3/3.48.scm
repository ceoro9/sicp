; This technic works, 'cause in that case 2 concurrent processes will try
; to acquire the same lock and since acquire operation is atomic, only of
; them is succeded, another one waits. So it eliminates the scenario, described
; in exercise.

(define (make-account id balance)

    ; ...

    (define (dispatch m)
        (cond ((eq? m 'get-id) id)
              ; ...
              (else (error "Unkwown operation on account" m)))))

(define (get-id account) (account 'get-id))

(define (serialized-exchange account1 account2)
    (let ((serializer1 (account1 'serializer))
          (serializer2 (account2 'serializer)))
    (if (< (get-id account1) (get-id account2))
        ((serializer1 (serializer2 exchange))
         account1
         account2)
        ((serializer2 (serializer1 exchange))
         account1
         account2))))

