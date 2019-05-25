(define (accumulate p start-value sequance)
    (define (iter c result)
        (if (null? c)
            result
            (iter (cdr c)
                  (p (car c) result))))
    (iter sequance start-value))

(define (enumerate-interval i j)
    (define (iter c result)
        (if (> c j)
            result
            (iter (+ c 1)
                  (append result (list c)))))
    (iter i (list)))

(define (flatmap p seq) (accumulate append '() (map p seq)))

(define empty-board (list (list (cons -1 -1))))

; Example of 2x2 board
; (1 . 1)  (1 . 2)
; (2 . 1)  (2 . 2)
; Solution: () - No way to put queens no board without check

; Example of 4x4 board
; (1 . 1)  (1 . 2)  (1 . 3)  (1 . 4)
; (2 . 1)  (2 . 2)  (2 . 3)  (2 . 4)
; (3 . 1)  (3 . 2)  (3 . 3)  (3 . 4)
; (4 . 1)  (4 . 2)  (4 . 3)  (4 . 4)
; Solution: (((2 . 1), (4 . 2), (1 . 3), (3 . 4)), ...)

(define (safe? k positions)
     (define (is-check-pos pos-1 pos-2)
        (OR (= (car pos-1) (car pos-2))             ; check rows
            (= (cdr pos-1) (cdr pos-2))             ; check columns
            (= (abs (- (car pos-1) (car pos-2)))    ; check diagonal 
               (abs (- (cdr pos-1) (cdr pos-2))))))

     (define (check-pos-to-others pos others)
        (if (null? others)
            #f
            (if (is-check-pos (car others) pos)
                #t
                (check-pos-to-others pos (cdr others)))))
     
     (define (check? current result)
         (if (OR (null? current) result)
            result
            (check? (cdr current)
                    (OR result 
                        (check-pos-to-others (car current)
                                             (cdr current))))))
    (NOT (check? positions #f)))

(define (adjoin-position new-row k others)
    (if (pair? (car (car others))) ; check if it's empty board
        (list (cons new-row k))
        (append others (list (cons new-row k)))))

(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter
                (lambda (positions) (safe? k positions))
                (flatmap
                    (lambda (new-row)
                        (map (lambda (rest-of-queens)
                            (adjoin-position
                                new-row k rest-of-queens))
                            (queen-cols (- k 1))))
                (enumerate-interval 1 board-size)))))

    (queen-cols board-size))

(display (queens 4)) 

; Complexity from T raises to board-size^board-size * T.
; Because queen-cols is going to be calculated for every 1...board-size
