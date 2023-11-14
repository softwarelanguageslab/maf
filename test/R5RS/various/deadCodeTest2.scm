(define (called n)
  (if (= n 0)
      10
      "hello"))

(called 0)
(called 1)

(define (constant-bool n)
  n
  #f)

(constant-bool 10)
20