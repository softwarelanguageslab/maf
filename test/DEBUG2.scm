(define v #t)
(define (f x) (set! v x))
(f (<change> #f v))