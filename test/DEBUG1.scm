(define x 5)
(define x-s (source x))
(define y 7)
(define z (+ x-s y))
(define z-s (sink z))

(define x-ss (sanitize x-s))
(define z2 (+ x-ss y))
(define z2-s (sink z2))