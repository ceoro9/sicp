(define (exchange account1 account2)
    (let ((difference (- (account1 'balance)
                         (account2 'balance))))
      ((account1 'withdraw) difference)
      ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
    (let ((serializer1 (account1 'serializer))
          (serializer2 (account2 'serializer)))
      ((serializer1 (serializer2 exchange))
                    account1
                    account2)))

; The main problem with such approach is that exchange procedure
; is serialized with the same serializers, it should use to withdraw
; and deposit money from accounts. So it turns out when serialized-exchange
; function was called, than exchange function was serialized with both acccount's
; serializers, there is no way withdraw money from first account and deposit money
; to second one, 'cause this operations are serialized with the same serializer, that
; exchange procedure was. 

