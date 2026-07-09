(define a 1)
(define b (let* ((w (* 3 5))
                (x (+ a 1))
                (y x))
            (+ x 3)))
(define c 5)
(let ((q (- c 2)))
    (set! a 3) ; overapproximation in the slice
    b)

        