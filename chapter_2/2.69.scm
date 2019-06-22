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

(define (addjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set)
                      (addjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
            (addjoin-set (make-leaf (car pair) (cadr pair))
                         (make-leaf-set (cdr pairs))))))

(define (successive-merge elements)
    (if (= (length elements) 1)
        (car elements)
        (successive-merge
            (addjoin-set (make-code-tree (car elements)
                                         (cadr elements))
                         (cddr elements)))))

(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs)))

(display (generate-huffman-tree (list
                                    (list 'A 5)
                                    (list 'B 3)
                                    (list 'C 2))))

