(define x 3)
(define (f n) 
    (+ 1 2)
    n)
(define a (f x))
(define b (f (begin x (+ 1 2))))
b
