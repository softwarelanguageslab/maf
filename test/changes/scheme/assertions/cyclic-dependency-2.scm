; Contains an R/W cycle: n -> m -> n.
(define (a n)
  (assert (integer? n))
  (b n))
(define (b m)
  (if (< 0 m)
       (a (- m 1))
       #t))
(a (<change> 10.5 10))
