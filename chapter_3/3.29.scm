;
; --a1-- !INVERTER! --s1-- |
;                          |
;                        !AND! --s3-- INVERTER --output--
;                          |
; --a2-- !INVERTER! --s2-- |


(define (or-gate a1 a2 output)
    (let ((s1 (make-wire))
          (s2 (make-wire))
          (s3 (make-wire)))
      (inverter a1 s1)
      (inverter a2 s2)
      (and-gate s1 s2 s3)
      (inverter s3 output)))

