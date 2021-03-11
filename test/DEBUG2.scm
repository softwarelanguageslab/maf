(define (our-great-test n)
  (assert (integer? n))
  (set! n (- n 1)))

(our-great-test (<change> 10.5 10))