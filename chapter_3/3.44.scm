(define (transfer from-account to-account amount)
    ((from-account 'withdraw) amount)
    ((to-account 'deposit) amount))

; > Is Louis right?
; Not really. Actually if withdraw and deposit operations on single account
; are serialized, such method of transfer is flawless. Since we were able to
; withdraw money from one account, even if the context was interrupted this does not
; really matter, later we will add this money to another account. This 2 operations are
; absolutely independent. The main problem here is that this "later" is not really determined.
; A small problem appears from user's side, when the money was withdrawed from one account and user
; sees this, but this sum does not appear on another account. It seems like the money just
; disappered. So it's better to make transfer operation atomic and make sure that money that
; was withdrawed from one account is delivered to another one.

; > If not, what is the essential difference between the transfer problem and the exchange problem?
; In exchanging, state of one account depends on the state of another. 
; So, while exchange procedure is executed, no concurrent operations
; on both accounts are allowed. But transfer method does have such restriction,
; because final state of both accounts depends only of their initial state and
; amount value.

