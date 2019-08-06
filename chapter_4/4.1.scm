; from left to right
(define (list-of-values exps env)
    (if (no-operands? exps)
    '()
    (let ((first-result (eval (first-operand exps) env)))
      (let ((rest-results (list-of-values (rest-operands exps) env)))
        (cons first-result rest-results)))))

; from right to left
(define (list-of-values exps env)
    (if (no-operands? exps)
    '()
    (let ((rest-results (list-of-values (rest-operands exps) env)))
      (let ((first-result (eval (first-operand exps) env)))
        (cons first-result rest-results)))))

