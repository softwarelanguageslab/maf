(define (a n)
  (assert (integer? n))
  (b n))

(define (b m)
  (if (= m 0)
      (a (- m 1))
      #t))

(a (<change> 10.5 10))