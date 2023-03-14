; x-s -> sanitizer -> z-s, no bad flows
(define x 5)
(define x-s (source x))
(define y 7)
(define x2-s (sanitize x-s))
(define z (+ x2-s y))
(define z-s (sink z))