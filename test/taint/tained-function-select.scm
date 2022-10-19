; Tainted flow from fun-select to target.
(define fun-select-base 1)
(define fun-select (source fun-select-base))
(define target 0)
(define (f) (set! target #t))
(define (g) (set! target "false"))
((list-ref (list f g) fun-select))
(sink target)