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

(define acc (make-account 100 'pass))

((acc 'pass 'withdraw) 20)

(display ((acc 'pass 'deposit) 100))

; ---
(display ((acc 'suck 'withdraw) 100))
(display ((acc 'suck 'withdraw) 100))
(display ((acc 'suck 'withdraw) 100))
(display ((acc 'suck 'withdraw) 100))
(display ((acc 'suck 'withdraw) 100))
(display ((acc 'suck 'withdraw) 100))
; call to police
(display ((acc 'suck 'withdraw) 100))

; ----
; no call to police
(display ((acc 'pass 'deposit) 100))
(display ((acc 'suck 'withdraw) 100))
(display ((acc 'suck 'withdraw) 100))
(display ((acc 'suck 'withdraw) 100))
(display ((acc 'suck 'withdraw) 100))
(display ((acc 'suck 'withdraw) 100))
(display ((acc 'suck 'withdraw) 100))
(display ((acc 'pass 'deposit) 100))
(display ((acc 'suck 'withdraw) 100))

