; Flow from x-s (= x marked as source) to z-s (= z marked as sink).
(define x 5)
(define x-s (source x))
(define y 7)
(define z (+ x-s y))
(define z-s (sink z))