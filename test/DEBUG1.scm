(define my-apply apply)
(define (apply f . args) (display f))
(my-apply + 1 2 3)