; Tainted flow from a2 to res.
(define a #t)
(define a2 (source a))
(define (b x) x)
(define (set-b)
  (set! b (lambda (x) #f)))
(if a2 (set-b))
(define res (b 10))
(sink res)