(define x 1)
(define y 4)
(define (f) (set! x (+ x y)))
(define (g) (let ((z x)) z 1))
(define (pastoe fun) (fun))
(pastoe f)
(pastoe g)
(f)
x

