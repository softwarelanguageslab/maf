(define x (+ 1 2))
(define y 8)
(define z (cons 1 2))
(set! x (begin (set! z 3) z))
x
