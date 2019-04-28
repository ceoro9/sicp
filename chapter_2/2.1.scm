(define (gcd a b)
    (define (min a b)
        (if (< a b)
            a
            b
        ))

    (define (max a b)
        (if (> a b)
            a
            b
        ))

    (define (gcd-iter a b)
        (if (= b 0)
            a
            (gcd-iter b (remainder a b))))

    (gcd-iter (max a b) (min a b)))

(define (make-rat n d)
    (let ((g (gcd n d)))
        (define result (cons (/ n g) (/ d g)))
        (cond ((and (< n 0) (> g 0)) result)
              ((and (> n 0) (< g 0)) (make-rat (* -1 n) 
                                               (* -1 d)))
              (else result))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))

(define (revert x)
    (make-rat (denom x) 
              (numer x)))

(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

(define (div-rat x y)
    (mul-rat x (revert y)))

(print-rat (make-rat 10 -5))
