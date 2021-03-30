(define (fun n)
  (assert (integer? n))
  (set! n (- n 1)))

(fun (<change> 10.5 10))