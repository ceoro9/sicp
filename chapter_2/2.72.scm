; in-symbols? is O(N)
; There are O(log N) branches to pass.
; Total complexity is O(N * log N)

; But, in our specific case with 2^n weights,
; we have absolutely unbalanced tree and number
; of branches is not log N, but just N.
; Therefore complexity becomes O(N ^ 2) in worst case,
; because obviosly we cannot have more branches
; than number of symbols in haffman tree.

; To encode the least frequent symbol we need O(N^2) steps,
; because we need to check every branch.

; To encode the most frequent symbol we need constant number of steps,
; because at first we are checking the symbols in left-branch,
; but in our case it's leaf, so we can find it with O(1) complexity
; and that's it, no more steps to describe. Total complexity is O(1).

(define (in-symbols? symbol tree)
    (if (leaf? tree)
        (eq? (symbol-leaf tree) symbol)
        (accumulate
            (lambda (a b) (or a b))
            #f
            (lambda (tree-symbol) (eq? tree-symbol symbol))
            (symbols tree))))

(define (encode-symbol symbol tree)
    (cond ((leaf? tree) '())
          ((in-symbols? symbol (left-branch tree))
              (cons 0 (encode-symbol symbol (left-branch tree))))
          ((in-symbols? symbol (right-branch tree))
              (cons 1 (encode-symbol symbol (right-branch tree))))
          (else (error "Could find this symbol in tree" symbol))))

