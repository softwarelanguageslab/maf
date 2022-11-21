; Tainted flow from a2 to b2.
(define a #t)
(define a2 (source a))
(define b #t)
(define (set-b) (set! b 10))
(if a2 (set-b))
(define b2 (sink b))