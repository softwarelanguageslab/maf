(define the-lambda '())

(define (update-lambda! n)
    (set! the-lambda (lambda () n)))

(update-lambda! 0)
(update-lambda! 1)

(define res (the-lambda))
res