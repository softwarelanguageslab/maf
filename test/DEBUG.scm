(define (fib n)
  (if (= n 0)
    n
    (let ((fib-n-1 (fib (- n 1)))
           (fib-n-2 (fib (- n 2))))
      (+ fib-n-1 fib-n-2))))
(fib 5)