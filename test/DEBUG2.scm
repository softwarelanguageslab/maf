(define a #t)
(define (set-a) (set! a #f))
(if a (set-a))