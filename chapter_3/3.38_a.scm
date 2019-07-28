; Peter: (set! balance (+ balance 10))
; Paul: (set! balance (- balance 20))
; Mary: (set! balance (- balance (/ balance 2)))

; 3! of combinations
; 1 2 3 : (100 + 10 - 20) / 2 = 45
; 1 3 2 : (100 + 10) / 2 - 20 = 35
; 2 1 3 : (100 - 20 + 10) / 2 = 45
; 2 3 1 : (100 - 20) / 2 + 10 = 50
; 3 1 2 : 100 / 2 + 10 - 20 = 40
; 3 2 1 : 100 / 2 - 20 + 10 = 40
;
; Possible balance values: 35, 40, 45, 50

