; (define acc (make-account 50))
; 
; E1 -> balance: 50, withdraw, deposit, dispatch --> Global
;                    |_________________________|
;                                |
;                           environment: E1
; 
; global env:
;   acc
;   |> ...
;   |> E1
;
;
; ((acc 'deposit) 40)
;
; E2 -> m: 'deposit --> E1
; E3 -> anount: 40 --> E1
;
; balance value is changed in E1 environment.
; E2, E3 are destroyed
;
;
; Question: Where is the local state for acc kept?
; Answer: They are kept in environments that are created when make-account is called
;
; Question: Which parts of the environment structure are shared between acc and acc2?
; Answer: global environment
;

