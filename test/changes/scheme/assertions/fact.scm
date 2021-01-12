(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))
(define res (<change> (fact 5) (number->string (fact 5))))
(assert (string? res))