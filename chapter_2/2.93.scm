; TODO: may be use message-passing instead of data-directed
; programming when working with polynomial terms,
; because technically there is no generic operations with terms,
; but it's just convinient for me to structure code like this.
(define (install-polynomial-terms-package)
    ; ...
    
    (define (quotient-terms T1 T2) (car (div-terms T1 T2)))
    (define (remainder-terms T1 T2) (cadr (div-terms T1 T2)))

    (define (gcd-terms T1 T2)
        (define (handle t1 t2)
            (if (empty-termlist? T2)
                T1
                (gcd-terms T2 (remainder-terms a b))))

        (cond ((empty-termlist? T1) (handle T2 T1))
              ((empty-termlist? T2) (handle T1 T2))
              ((> (order (first-term T1))
                  (order (first-term T2)))
                    (handle T1 T2))
              (else (handle T2 T2))))
    
    (put 'gcd '(term term) gcd-terms)
    (put 'quotient '(term term) quotient-terms)
    (put 'remainder '(term term) remainder-terms))


(define (gcd-terms t1 t2) (apply-generic 'gcd t1 t2))
(define (quotient-terms t1 t2) (apply-generic 'quotient t1 t2))
(define (remainder-terms t2 t2) (apply-generic 'remainder t1 t2))

(define (install-rational-package)
    
    (define (tag s) (attach-tag 'rat s))

    (define (make num den) (cons num den))
    (define (numerator r) (car r))
    (define (denumerator r) (cdr r))

    (define (reduce r)
        (let ((numerator-term-list (term-list (numerator r)))
              (denumerator-term-list (term-list (denumerator r))))
            (let ((common-term-list
                (gcd-terms numerator-term-list
                           denumerator-term-list)))
                (make (quotient-terms numerator-term-list
                                      common-term-list)
                      (quotient-terms denumerator-term-list
                                      common-term-list)))))
  
    (define (add R1 R2)
        (let ((r1-numerator-term-list (term-list (numerator R1)))
              (r1-denumerator-term-list (term-list (denumerator R1)))
              (r2-numerator-term-list (term-list (numerator R2)))
              (r2-dumerator-term-list (term-list (denumerator R2))))
            (make
                (add-terms
                    (multiply-terms r1-numerator-term-list
                                    r2-denumerator-term-list)
                    (multiply-terms r2-numerator-term-list
                                    r1-denumerator-term-list))
                (multiply-terms r1-denumerator-term-list
                                r2-denumerator-term-list))))

    (put 'make '(rational) (lambda (n d) (tag (reduce (make n d)))))
    (put 'add '(rational rational) (lambda (a b) (tag (add a b)))))

(define (make-rational num dem) ((get 'make '(rational)) num dem))
(define (add-rational r1 r2) (apply-generic 'add r1 r2))
 
