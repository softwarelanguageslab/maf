;; OK

(define (fib n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

(provide/contract
 (fib (-> number?  number?)))

(@unchecked fib OPQ)
(safe)
