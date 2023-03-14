; Contains an R/W cycle: n -> n.
(define (mk-list n)
   (assert (integer? n))
   (set! n (- n 1)))
(mk-list (<change> 10.5 10))