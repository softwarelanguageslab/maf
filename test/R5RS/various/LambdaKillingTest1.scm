(define (fac n)
  (if (= n 0)
      1
      (* n (fac (- n 1)))))

(define (math1)
  (+ 0 0)
  (fac 3))

(define (math2)
  (* 0 0)
  (fac 5))

(define (does-nothing arg1 arg2)
  -1)

(math1)
(math2)

(does-nothing fac fac)