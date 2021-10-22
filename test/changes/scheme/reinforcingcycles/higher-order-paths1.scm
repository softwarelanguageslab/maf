(define (phi x) (not x))
(define (chi x) x)
(or ((<change> phi chi) #t)
    ((<change> phi chi) #f))