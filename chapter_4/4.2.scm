; A)
; > A procedure application is any compound expression that is not one of the above expression types.
; Since we place application clause before definition,
; evaluation of (define x 3) expression will be caught by
; application handler and it will try to call define method
; on x and 3 arguments resulting in inappropriate behavior.
;
; B)

(define (application? exp) (tagged-list exp 'call))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

