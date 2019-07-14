(define (call-the-cops)
    (display "I AM CALLING THE POLICE"))

(define (make-account balance password)
    (define incorrect-password-tries 0)

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
            (begin
                (set! incorrect-password-tries 0)
                (cond ((eq? op 'withdraw) withdraw)
                      ((eq? op 'deposit) deposit)
                      (else (error "Unknow operation"))))
            (begin
                (set! incorrect-password-tries (+ incorrect-password-tries 1))
                (if (= incorrect-password-tries 7)
                    (begin
                        (set! incorrect-password-tries 0)
                        (call-the-cops)))
                (error "Wrong password"))))
    dispatch)

(define (make-join target-acc target-password new-password)
    (if (number? ((target-acc target-password 'withdraw) 0))
        (lambda (password op)
            (if (eq? password new-password)
                (target-acc target-password op)
                (target-acc 'super-random-password op)))
        (error "Wrong target account password")))

(define mike-acc (make-account 100 'pass))
(define peter-acc (make-join mike-acc 'pass 'new-pass))

(display ((peter-acc 'new-pass 'withdraw) 98))
(newline)
(display ((mike-acc 'pass 'withdraw) 1))
(newline)
(display ((mike-acc 'wrong-pass 'withdraw) 1))

