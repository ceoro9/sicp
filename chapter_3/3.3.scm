(define (make-account balance password)
    (define (withdraw amount)
        (if (< amount balance)
            (begin
                (set! balance (- balance amount))
                balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (define (dispatch inputed-password op)
        (if (equal? inputed-password password)
            (cond ((eq? op 'withdraw) withdraw)
                  ((eq? op 'deposit) deposit)
                  (else (error "Unknow operation")))
            (error "Wrong password")))
    dispatch)

(define acc (make-account 100 'pass))

((acc 'pass 'withdraw) 20)

(display ((acc 'pass 'deposit) 100))

; wrong
(display ((acc 'suck 'withdraw) 100))

