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


(define W-A        'A)
(define W-BOOM     'B)
(define W-GET      'G)
(define W-JOB      'J)
(define W-SHA      'S)
(define W-NA       'N)
(define W-WAH      'W)
(define W-YIP      'Y)
(define W-SPACE    'E)
(define W-NEW-LINE 'L)

; map word on symbols
(define (map-wos word)
    (cond ((equal? word "A")    W-A)
          ((equal? word "BOOM") W-BOOM)
          ((equal? word "GET")  W-GET)
          ((equal? word "JOB")  W-JOB)
          ((equal? word "SHA")  W-SHA)
          ((equal? word "NA")   W-NA)
          ((equal? word "WAH")  W-WAH)
          ((equal? word "YIP")  W-YIP)
          ((equal? word " ")    W-SPACE)
          ((equal? word "\n")   W-NEW-LINE)
          (else (error "Unknown word" word))))

; map symbols on words
(define (map-sow symbol)
    (cond ((eq? symbol W-A)        "A")
          ((eq? symbol W-BOOM)     "BOOM")
          ((eq? symbol W-GET)      "GET")
          ((eq? symbol W-JOB)      "JOB")
          ((eq? symbol W-SHA)      "SHA")
          ((eq? symbol W-NA)       "NA")
          ((eq? symbol W-WAH)      "WAH")
          ((eq? symbol W-YIP)      "YIP")
          ((eq? symbol W-SPACE)    " ")
          ((eq? symbol W-NEW-LINE) "\n")
          (else (error "Unknown symbol" symbol))))

(define rock-haffman-tree
    (generate-huffman-tree (list
                             (list (map-wos "A")    2)
                             (list (map-wos "BOOM") 1)
                             (list (map-wos "GET")  2)
                             (list (map-wos "JOB")  2)
                             (list (map-wos "SHA")  3)
                             (list (map-wos "NA")   16)
                             (list (map-wos "WAH")  1)
                             (list (map-wos "YIP")  9)
                             (list (map-wos " ")    10)
                             (list (map-wos "\n")   3))))

(define rock-song (list
                    (map-wos "GET")
                    (map-wos " ")
                    (map-wos "JOB")
                    (map-wos "\n")
                    (map-wos "SHA")
                    (map-wos " ")
                    (map-wos "NA")
                    (map-wos " ")
                    (map-wos "NA")
                    (map-wos " ")
                    (map-wos "NA")
                    (map-wos " ")
                    (map-wos "NA")
                    (map-wos " ")
                    (map-wos "NA")
                    (map-wos " ")
                    (map-wos "\n")
                    (map-wos "GET")
                    (map-wos " ")
                    (map-wos "JOB")
                    (map-wos "\n")
                    ; ...
                  ))

(define encoded-message (encode rock-song rock-haffman-tree))
    
(display "ENCODED MESSAAGE: ")
(display encoded-message)

(define decoded-message (decode encoded-message rock-haffman-tree))

(newline)
(display "DECODED MESSAGE: \n")
(for-each (lambda (symbol) (display (map-sow symbol))) decoded-message)



(define (tree-depth tree)
    (if (leaf? tree)
        1
        (max (+ 1 (tree-depth (left-branch tree)))
             (+ 1 (tree-depth (right-branch tree))))))

(newline)
(newline)
(display "MAXIMUM NUMBER OF BITS TO ENCODE WORD: ")
(display (tree-depth rock-haffman-tree))


(newline)
(newline)
(display "NUMBER OF BITS TO ENCODE SONG: ")
(display (length encoded-message))

(newline)
(display "NUMBER OF BITS TO ENCODE WITH FIXED-LENGTH ENCODING: ")
(display (* 3 (length rock-song)))


; )
