(define (accumulate op base func elements)
    (if (null? elements)
        base
        (op (func (car elements))
            (accumulate op base func (cdr elements)))))

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
    (list left
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))
(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))
(define (in-symbols? symbol tree)
    (if (leaf? tree)
        (eq? (symbol-leaf tree) symbol)
        (accumulate
            (lambda (a b) (or a b))
            #f
            (lambda (tree-symbol) (eq? tree-symbol symbol))
            (symbols tree))))

(define (decode bits tree)
    (define (choose-branch bit branch)
        (cond ((= bit 0) (left-branch branch))
              ((= bit 1) (right-branch branch))
              (else (error "bad bit: CHOOSE-BRANCH " bit))))

    (define (decode-1 bits current-branch)
        (if (null? bits)
            '()
            (let ((next-branch (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch)
                          (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch)))))

    (decode-1 bits tree))

(define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree
                        (make-leaf 'B 2)
                        (make-code-tree (make-leaf 'D 1)
                                        (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (encode-symbol symbol tree)
    (cond ((leaf? tree) '())
          ((in-symbols? symbol (left-branch tree))
              (cons 0 (encode-symbol symbol (left-branch tree))))
          ((in-symbols? symbol (right-branch tree))
              (cons 1 (encode-symbol symbol (right-branch tree))))
          (else (error "Could find this symbol in tree" symbol))))

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))

(define text-message (decode sample-message sample-tree))
(display (encode text-message sample-tree))
(newline)
(display sample-message)
