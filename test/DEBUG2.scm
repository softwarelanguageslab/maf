; The incremental update will create a new cyclic flow in which immediately the outdated value of `v` is introduced.
(define v #t)
(define (f x) (set! v x))
(f (<change> #f v))