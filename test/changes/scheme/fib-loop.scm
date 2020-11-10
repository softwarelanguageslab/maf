(define (fib n) ;; Simple Fibonacci
  (if (= n 0)
    n
    (let ((fib-n-1 (fib (- n 1)))
           (fib-n-2 (fib (- n 2))))
      (+ fib-n-1 fib-n-2))))

(define (fib-loop n)
  (define (loop i)
    (if (< i n)
      (begin
        (display (fib i))
        (display " ")
        (loop (+ i 1)))))
  (loop 0))

(<change> (fib-loop 10) (fib 10))