; Tainted flow from a2 to b2.
(define a #t)
(define a2 (source a))
(define (b x) x)
(define (set-b) (set! b (lambda (x) #f)))
(define res
  (begin
    (if a2 (set-b))
    (b 10)))
(define b2 (sink res))