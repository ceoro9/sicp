; There is no sense to protect the balance accessor, 'cause in any period of time
; balance value is 100% valid, since all write operations are serialized. So even
; if requester gets stale balance value(it was changed between accessor
; read value of balance and it was returned to requester), that value was valid
; some time ago, but what is important that this is a valid value.

