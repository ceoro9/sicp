; (define W1 ...)
;
; E1 -> initial-amount: 100 --> Global
; E2 -> balance: 100 --> E1
;
; 
; global env:
;   W1
;   | -> body: (if (>= balance amount) ...)
;   | -> environment: E2
;
; --------------------------------------------
;
; (define W2 ...)
;
; E3 -> initial-amount: 100 --> Global
; E4 -> balance: 100 --> E3
;
; global env:
;   W2
;   | -> body: (if (>= balance amount) ...)
;   | -> environment: E4
;
; --------------------------------------------
;
; (W1 50)
; This affects balance variable in frame of E2 environment.

