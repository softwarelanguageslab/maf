(define a 1)
(define d 4)
(define b (let ((w (* d 5))
                (x (+ a 1)))
            (+ x 3)))
(define c 5)
(let ((q (- c 2)))
    (set! a 3) 
    b)

        