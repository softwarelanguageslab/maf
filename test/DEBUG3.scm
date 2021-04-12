(define x 1)
(define y 2)
(define (write) (<change> (set! x 7) (set! y 7)))
(define (read) (<change> x y))

(read)
(write)