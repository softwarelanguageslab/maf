(define (a x) x)

(define (f) (a 1))
(define (g) (a (<change> #t 'ok)))

(f)
(g)