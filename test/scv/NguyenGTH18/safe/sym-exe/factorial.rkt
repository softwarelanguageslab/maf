(define (factorial n)
  (if (zero? n) 1 (* n (factorial (sub1 n)))))

(provide/contract
 (factorial (-> number? number?)))

(@unchecked factorial OPQ)
(safe)
