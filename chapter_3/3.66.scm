; (1, 1) (1, 2) (1, 3) ... (1, N)
;        (2, 2) (2, 3) ... (2, N)
;               (3, 3) ... (3, N)
; ...
;                          (N, N)

(define result (pairs integers integers))

; result
; (1, 1) (1, 2) ... (1, 3) ... (1, N) ...
;
; > For example, approximately how many pairs precede the pair (1, 100)?
; (1, 1) ... (1, 99) - 99
; pairs between (1, 2) and (1, 100) - 98
; answer: 197
;
;
; > the pair (99, 100)? the pair (100, 100)?
;
; last 2 rows
; (99, 99)  (99, 100)
;           (100, 100)
;
; result stream: ... (99, 99) (99 100) (100, 100)
;
; (99, 100) = 100 * 100 / 2 - 1
; (100, 100) = 100 * 100 / 2

